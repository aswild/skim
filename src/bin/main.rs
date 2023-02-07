extern crate clap;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate atty;
extern crate shlex;
extern crate skim;
extern crate time;

use derive_builder::Builder;
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};

use anyhow::Context;
use clap::{crate_version, Arg, ArgAction, ArgMatches, Command};

use skim::prelude::*;

const USAGE: &str = "
Usage: sk [options]

  Options
    -h, --help           print this help menu
    --version            print out the current version of skim

  Search
    --tac                reverse the order of search result
    --no-sort            Do not sort the result
    -t, --tiebreak [score,begin,end,-score,length...]

                         comma seperated criteria
    -n, --nth 1,2..5     specify the fields to be matched
    --with-nth 1,2..5    specify the fields to be transformed
    -d, --delimiter \\t  specify the delimiter(in REGEX) for fields
    -e, --exact          start skim in exact mode
    --regex              use regex instead of fuzzy match
    --algo=TYPE          Fuzzy matching algorithm:
                         [skim_v1|skim_v2|clangd] (default: skim_v2)
    --case [respect,ignore,smart] (default: smart)
                         case sensitive or not

  Interface
    -b, --bind KEYBINDS  comma seperated keybindings, in KEY:ACTION
                         such as 'ctrl-j:accept,ctrl-k:kill-line'
    -m, --multi          Enable Multiple Selection
    --no-multi           Disable Multiple Selection
    --no-mouse           Disable mouse events
    -c, --cmd ag         command to invoke dynamically
    -i, --interactive    Start skim in interactive(command) mode
    --color [BASE][,COLOR:ANSI]
                         change color theme
    --no-hscroll         Disable horizontal scroll
    --keep-right         Keep the right end of the line visible on overflow
    --skip-to-pattern    Line starts with the start of matched pattern
    --no-clear-if-empty  Do not clear previous items if command returns empty result
    --show-cmd-error     Send command error message if command fails

  Layout
    --layout=LAYOUT      Choose layout: [default|reverse|reverse-list]
    --height=HEIGHT      Height of skim's window (--height 40%)
    --no-height          Disable height feature
    --min-height=HEIGHT  Minimum height when --height is given by percent
                         (default: 10)
    --auto-height        Shrink height when stdin input contains fewer than
                         HEIGHT lines.
    --margin=MARGIN      Screen Margin (TRBL / TB,RL / T,RL,B / T,R,B,L)
                         e.g. (sk --margin 1,10%)
    -p, --prompt '> '    prompt string for query mode
    --cmd-prompt '> '    prompt string for command mode

  Display
    --ansi               parse ANSI color codes for input strings
    --tabstop=SPACES     Number of spaces for a tab character (default: 8)
    --inline-info        Display info next to query
    --header=STR         Display STR next to info
    --header-lines=N     The first N lines of the input are treated as header

  History
    --history=FILE       History file
    --history-size=N     Maximum number of query history entries (default: 1000)
    --cmd-history=FILE   command History file
    --cmd-history-size=N Maximum number of command history entries (default: 1000)

  Preview
    --preview=COMMAND    command to preview current highlighted line ({})
                         We can specify the fields. e.g. ({1}, {..3}, {0..})
    --preview-window=OPT Preview window layout (default: right:50%)
                         [up|down|left|right][:SIZE[%]][:hidden][:+SCROLL[-OFFSET]]

  Scripting
    -q, --query \"\"       specify the initial query
    --cmd-query \"\"       specify the initial query for interactive mode
    --expect KEYS        comma seperated keys that can be used to complete skim
    --read0              Read input delimited by ASCII NUL(\\0) characters
    --print0             Print output delimited by ASCII NUL(\\0) characters
    --no-clear           Do not clear screen on exit
    --print-query        Print query as the first line
    --print-cmd          Print command query as the first line (after --print-query)
    --print-score        Print matching score in filter output (with --filter)
    -1, --select-1       Automatically select the only match
    -0, --exit-0         Exit immediately when there's no match
    --sync               Synchronous search for multi-staged filtering
    --pre-select-n=NUM   Pre-select the first n items in multi-selection mode
    --pre-select-pat=REGEX
                         Pre-select the matched items in multi-selection mode
    --pre-select-items=$'item1\\nitem2'
                         Pre-select the items separated by newline character
    --pre-select-file=FILENAME
                         Pre-select the items read from file

  Environment variables
    SKIM_DEFAULT_COMMAND Default command to use when input is tty
    SKIM_DEFAULT_OPTIONS Default options (e.g. '--ansi --regex')
                         You should not include other environment variables
                         (e.g. '-c \"$HOME/bin/ag\"')

  Removed
    -I replstr           replace `replstr` with the selected item

  Reserved (not used for now)
    --extended
    --literal
    --cycle
    --hscroll-off=COL
    --filepath-word
    --jump-labels=CHARS
    --border
    --no-bold
    --info
    --pointer
    --marker
    --phony
";

const DEFAULT_HISTORY_SIZE: usize = 1000;

//------------------------------------------------------------------------------
fn main() {
    env_logger::builder().format_timestamp_nanos().init();

    match real_main() {
        Ok(exit_code) => std::process::exit(exit_code),
        Err(err) => {
            // if downstream pipe is closed, exit silently, see PR#279
            if let Some(ioerr) = err.downcast_ref::<io::Error>() {
                if ioerr.kind() == std::io::ErrorKind::BrokenPipe {
                    std::process::exit(0)
                }
            }
            eprintln!("Error: {err:#}");
            std::process::exit(2)
        }
    }
}

#[rustfmt::skip]
fn real_main() -> anyhow::Result<i32> {
    let mut stdout = std::io::stdout();

    let mut args = Vec::new();

    args.push(env::args().next().expect("there should be at least one arg: the application name"));
    args.extend(env::var("SKIM_DEFAULT_OPTIONS")
        .ok()
        .and_then(|val| shlex::split(&val))
        .unwrap_or_default());
    for arg in env::args().skip(1) {
        args.push(arg);
    }


    //------------------------------------------------------------------------------
    // parse options
    let opts = Command::new("sk")
        .author("Jinzhou Zhang<lotabout@gmail.com>")
        .version(crate_version!())
        .args_override_self(true)
        .disable_help_flag(true)
        .arg(Arg::new("help").long("help").short('h').action(ArgAction::SetTrue))
        .arg(Arg::new("bind").long("bind").short('b').action(ArgAction::Append))
        .arg(Arg::new("multi").long("multi").short('m').action(ArgAction::SetTrue).overrides_with("no-multi"))
        .arg(Arg::new("no-multi").long("no-multi").action(ArgAction::SetTrue).overrides_with("multi"))
        .arg(Arg::new("prompt").long("prompt").short('p').default_value("> "))
        .arg(Arg::new("cmd-prompt").long("cmd-prompt").default_value("c> "))
        .arg(Arg::new("expect").long("expect").action(ArgAction::Append).value_name("KEYS"))
        .arg(Arg::new("tac").long("tac").action(ArgAction::SetTrue))
        .arg(Arg::new("tiebreak").long("tiebreak").short('t'))
        .arg(Arg::new("ansi").long("ansi").action(ArgAction::SetTrue))
        .arg(Arg::new("exact").long("exact").short('e').action(ArgAction::SetTrue))
        .arg(Arg::new("cmd").long("cmd").short('c'))
        .arg(Arg::new("interactive").long("interactive").short('i').action(ArgAction::SetTrue))
        .arg(Arg::new("query").long("query").short('q'))
        .arg(Arg::new("cmd-query").long("cmd-query"))
        .arg(Arg::new("regex").long("regex").action(ArgAction::SetTrue))
        .arg(Arg::new("delimiter").long("delimiter").short('d'))
        .arg(Arg::new("nth").long("nth").short('n'))
        .arg(Arg::new("with-nth").long("with-nth"))
        .arg(Arg::new("replstr").short('I'))
        .arg(Arg::new("color").long("color"))
        .arg(Arg::new("margin").long("margin").default_value("0,0,0,0"))
        .arg(Arg::new("min-height").long("min-height").default_value("10"))
        .arg(Arg::new("height").long("height").default_value("100%"))
        .arg(Arg::new("no-height").long("no-height").action(ArgAction::SetTrue))
        .arg(Arg::new("auto-height").long("auto-height").action(ArgAction::SetTrue))
        .arg(Arg::new("no-clear").long("no-clear").action(ArgAction::SetTrue))
        .arg(Arg::new("no-mouse").long("no-mouse").action(ArgAction::SetTrue))
        .arg(Arg::new("preview").long("preview"))
        .arg(Arg::new("preview-window").long("preview-window").default_value("right:50%"))
        .arg(Arg::new("reverse").long("reverse").action(ArgAction::SetTrue))
        .arg(Arg::new("algorithm").long("algo").default_value("skim_v2"))
        .arg(Arg::new("case").long("case").default_value("smart"))
        .arg(Arg::new("literal").long("literal").action(ArgAction::SetTrue))
        .arg(Arg::new("cycle").long("cycle").action(ArgAction::SetTrue))
        .arg(Arg::new("no-hscroll").long("no-hscroll").action(ArgAction::SetTrue))
        .arg(Arg::new("hscroll-off").long("hscroll-off").default_value("10"))
        .arg(Arg::new("filepath-word").long("filepath-word").action(ArgAction::SetTrue))
        .arg(Arg::new("jump-labels").long("jump-labels").default_value("abcdefghijklmnopqrstuvwxyz"))
        .arg(Arg::new("border").long("border").action(ArgAction::SetTrue))
        .arg(Arg::new("inline-info").long("inline-info").action(ArgAction::SetTrue))
        .arg(Arg::new("header").long("header").default_value(""))
        .arg(Arg::new("header-lines").long("header-lines").value_parser(clap::value_parser!(usize)).default_value("0"))
        .arg(Arg::new("tabstop").long("tabstop").default_value("8"))
        .arg(Arg::new("no-bold").long("no-bold").action(ArgAction::SetTrue))
        .arg(Arg::new("history").long("history"))
        .arg(Arg::new("cmd-history").long("cmd-history"))
        .arg(Arg::new("history-size").long("history-size").default_value("1000"))
        .arg(Arg::new("cmd-history-size").long("cmd-history-size").default_value("1000"))
        .arg(Arg::new("print-query").long("print-query").action(ArgAction::SetTrue))
        .arg(Arg::new("print-cmd").long("print-cmd").action(ArgAction::SetTrue))
        .arg(Arg::new("print-score").long("print-score").action(ArgAction::SetTrue))
        .arg(Arg::new("read0").long("read0").action(ArgAction::SetTrue))
        .arg(Arg::new("print0").long("print0").action(ArgAction::SetTrue))
        .arg(Arg::new("sync").long("sync").action(ArgAction::SetTrue))
        .arg(Arg::new("extended").long("extended").short('x').action(ArgAction::SetTrue))
        .arg(Arg::new("no-sort").long("no-sort").action(ArgAction::SetTrue))
        .arg(Arg::new("select-1").long("select-1").short('1').action(ArgAction::SetTrue))
        .arg(Arg::new("exit-0").long("exit-0").short('0').action(ArgAction::SetTrue))
        .arg(Arg::new("filter").long("filter").short('f'))
        .arg(Arg::new("layout").long("layout").default_value("default"))
        .arg(Arg::new("keep-right").long("keep-right").action(ArgAction::SetTrue))
        .arg(Arg::new("skip-to-pattern").long("skip-to-pattern").default_value(""))
        .arg(Arg::new("pre-select-n").long("pre-select-n").value_parser(clap::value_parser!(usize)).default_value("0"))
        .arg(Arg::new("pre-select-pat").long("pre-select-pat").default_value(""))
        .arg(Arg::new("pre-select-items").long("pre-select-items"))
        .arg(Arg::new("pre-select-file").long("pre-select-file").default_value(""))
        .arg(Arg::new("no-clear-if-empty").long("no-clear-if-empty").action(ArgAction::SetTrue))
        .arg(Arg::new("show-cmd-error").long("show-cmd-error").action(ArgAction::SetTrue))
        .get_matches_from(args);

    // TODO remove this
    if opts.get_flag("help") {
        write!(stdout, "{}", USAGE)?;
        return Ok(0);
    }

    //------------------------------------------------------------------------------
    let mut options = parse_options(&opts)?;

    options.preview_window = opts.get_one("preview-window").map(String::as_str);

    //------------------------------------------------------------------------------
    // initialize collector
    let item_reader_option = SkimItemReaderOption::default()
        .ansi(opts.get_flag("ansi"))
        .delimiter(opts.get_one("delimiter").map(String::as_str).unwrap_or(""))
        .with_nth(opts.get_one("with-nth").map(String::as_str).unwrap_or(""))
        .nth(opts.get_one("nth").map(String::as_str).unwrap_or(""))
        .read0(opts.get_flag("read0"))
        .show_error(opts.get_flag("show-cmd-error"))
        .build();

    let cmd_collector = Rc::new(RefCell::new(SkimItemReader::new(item_reader_option)));
    options.cmd_collector = cmd_collector.clone();

    //------------------------------------------------------------------------------
    // read in the history file
    let fz_query_histories = opts.get_one("history").map(String::as_str);
    let cmd_query_histories = opts.get_one("cmd-history").map(String::as_str);
    let query_history = fz_query_histories.and_then(|filename| read_file_lines(filename).ok()).unwrap_or_default();
    let cmd_history = cmd_query_histories.and_then(|filename| read_file_lines(filename).ok()).unwrap_or_default();

    if fz_query_histories.is_some() || cmd_query_histories.is_some() {
        options.query_history = &query_history;
        options.cmd_history = &cmd_history;
        // bind ctrl-n and ctrl-p to handle history
        options.bind.insert(0, "ctrl-p:previous-history,ctrl-n:next-history");
    }

    //------------------------------------------------------------------------------
    // handle pre-selection options
    let pre_select_n: Option<usize> = opts.get_one("pre-select-n").copied();
    let pre_select_pat = opts.get_one("pre-select-pat").map(String::as_str);
    let pre_select_items: Option<Vec<String>> = opts.get_one::<String>("pre-select-items").map(|s| s.split('\n').map(String::from).collect());
    let pre_select_file = opts.get_one("pre-select-file").map(String::as_str);

    if pre_select_n.is_some() || pre_select_pat.is_some() || pre_select_items.is_some() || pre_select_file.is_some() {
        let first_n = pre_select_n.unwrap_or(0);
        let pattern = pre_select_pat.unwrap_or("");
        let preset_items = pre_select_items.unwrap_or_default();
        let preset_file = pre_select_file.and_then(|filename| read_file_lines(filename).ok()).unwrap_or_default();

        let selector = DefaultSkimSelector::default()
            .first_n(first_n)
            .regex(pattern)
            .preset(preset_items)
            .preset(preset_file);
        options.selector = Some(Rc::new(selector));
    }

    //------------------------------------------------------------------------------
    let bin_options = BinOptionsBuilder::default()
        .filter(opts.get_one("filter").map(String::as_str))
        .print_query(opts.get_flag("print-query"))
        .print_cmd(opts.get_flag("print-cmd"))
        .output_ending(if opts.get_flag("print0") { "\0" } else { "\n" })
        .build()
        .expect("");

    //------------------------------------------------------------------------------
    // read from pipe or command
    let rx_item: Option<SkimItemReceiver> = (|| {
        if atty::isnt(atty::Stream::Stdin) {
            Some(match stdin_autoheight_reader(&mut options) {
                Some(reader) => cmd_collector.borrow().of_bufread(reader),
                None => cmd_collector.borrow().of_bufread(BufReader::new(io::stdin())),
            })
        } else {
            None
        }
    })();

    let options = options;

    //------------------------------------------------------------------------------
    // filter mode
    if opts.contains_id("filter") {
        return filter(&bin_options, &options, rx_item).map_err(Into::into);
    }

    //------------------------------------------------------------------------------
    let output = Skim::run_with(&options, rx_item);
    if output.is_none() { // error
        return Ok(135);
    }

    //------------------------------------------------------------------------------
    // output
    let output = output.unwrap();
    if output.is_abort {
        return Ok(130);
    }

    // output query
    if bin_options.print_query {
        write!(stdout, "{}{}", output.query, bin_options.output_ending)?;
    }

    if bin_options.print_cmd {
        write!(stdout, "{}{}", output.cmd, bin_options.output_ending)?;
    }

    if opts.contains_id("expect") {
        match output.final_event {
            Event::EvActAccept(Some(accept_key)) => {
                write!(stdout, "{}{}", accept_key, bin_options.output_ending)?;
            }
            Event::EvActAccept(None) => {
                write!(stdout, "{}", bin_options.output_ending)?;
            }
            _ => {}
        }
    }

    for item in output.selected_items.iter() {
        write!(stdout, "{}{}", item.output(), bin_options.output_ending)?;
    }

    //------------------------------------------------------------------------------
    // write the history with latest item
    if let Some(file) = fz_query_histories {
        let limit = opts.get_one("history-size").map(String::as_str)
            .and_then(|size| size.parse::<usize>().ok())
            .unwrap_or(DEFAULT_HISTORY_SIZE);
        write_history_to_file(&query_history, &output.query, limit, file)?;
    }

    if let Some(file) = cmd_query_histories {
        let limit = opts.get_one("cmd-history-size").map(String::as_str)
            .and_then(|size| size.parse::<usize>().ok())
            .unwrap_or(DEFAULT_HISTORY_SIZE);
        write_history_to_file(&cmd_history, &output.cmd, limit, file)?;
    }

    Ok(if output.selected_items.is_empty() { 1 } else { 0 })
}

fn parse_options(options: &ArgMatches) -> anyhow::Result<SkimOptions<'_>> {
    Ok(SkimOptionsBuilder::default()
        .color(options.get_one("color").map(String::as_str))
        .min_height(
            options
                .get_one::<String>("min-height")
                .map(|s| s.parse().context("can't parse min-height option"))
                .transpose()?
                .unwrap_or_default(),
        )
        .height(
            options
                .get_one::<String>("height")
                .map(|s| s.parse().context("can't parse height option"))
                .transpose()?
                .unwrap_or_default(),
        )
        .no_height(options.get_flag("no-height"))
        .auto_height(options.get_flag("auto-height"))
        .margin(options.get_one("margin").map(String::as_str))
        .preview(options.get_one("preview").map(String::as_str))
        .cmd(options.get_one("cmd").map(String::as_str))
        .query(options.get_one("query").map(String::as_str))
        .cmd_query(options.get_one("cmd-query").map(String::as_str))
        .interactive(options.get_flag("interactive"))
        .prompt(options.get_one("prompt").map(String::as_str))
        .cmd_prompt(options.get_one("cmd-prompt").map(String::as_str))
        .bind(
            options
                .get_many("bind")
                .map(|values| values.map(String::as_str).collect::<Vec<_>>())
                .unwrap_or_default(),
        )
        .expect(
            options
                .get_many("expect")
                .map(|values| values.map(String::as_str).collect::<Vec<_>>().join(",")),
        )
        .multi(if options.get_flag("no-multi") {
            false
        } else {
            options.get_flag("multi")
        })
        .layout(options.get_one("layout").map(String::as_str).unwrap_or(""))
        .reverse(options.get_flag("reverse"))
        .no_hscroll(options.get_flag("no-hscroll"))
        .no_mouse(options.get_flag("no-mouse"))
        .no_clear(options.get_flag("no-clear"))
        .tabstop(options.get_one("tabstop").map(String::as_str))
        .tiebreak(options.get_one("tiebreak").cloned())
        .tac(options.get_flag("tac"))
        .nosort(options.get_flag("no-sort"))
        .exact(options.get_flag("exact"))
        .regex(options.get_flag("regex"))
        .delimiter(options.get_one("delimiter").map(String::as_str))
        .inline_info(options.get_flag("inline-info"))
        .header(options.get_one("header").map(String::as_str))
        .header_lines(options.get_one("header-lines").copied().unwrap_or(0))
        .layout(options.get_one("layout").map(String::as_str).unwrap_or(""))
        .algorithm(FuzzyAlgorithm::of(
            options.get_one("algorithm").map(String::as_str).unwrap(),
        ))
        .case(match options.get_one("case").map(String::as_str) {
            Some("smart") => CaseMatching::Smart,
            Some("ignore") => CaseMatching::Ignore,
            _ => CaseMatching::Respect,
        })
        .keep_right(options.get_flag("keep-right"))
        .skip_to_pattern(options.get_one("skip-to-pattern").map(String::as_str).unwrap_or(""))
        .select1(options.get_flag("select-1"))
        .exit0(options.get_flag("exit-0"))
        .sync(options.get_flag("sync"))
        .no_clear_if_empty(options.get_flag("no-clear-if-empty"))
        .read0(options.get_flag("read0"))
        .build())
}

fn read_file_lines(filename: &str) -> Result<Vec<String>, std::io::Error> {
    let file = File::open(filename)?;
    let ret = BufReader::new(file).lines().collect();
    debug!("file content: {:?}", ret);
    ret
}

fn write_history_to_file(
    orig_history: &[String],
    latest: &str,
    limit: usize,
    filename: &str,
) -> Result<(), std::io::Error> {
    if orig_history.last().map(|l| l.as_str()) == Some(latest) {
        // no point of having at the end of the history 5x the same command...
        return Ok(());
    }
    let additional_lines = if latest.trim().is_empty() { 0 } else { 1 };
    let start_index = if orig_history.len() + additional_lines > limit {
        orig_history.len() + additional_lines - limit
    } else {
        0
    };

    let mut history = orig_history[start_index..].to_vec();
    history.push(latest.to_string());

    let file = File::create(filename)?;
    let mut file = BufWriter::new(file);
    file.write_all(history.join("\n").as_bytes())?;
    Ok(())
}

#[derive(Builder)]
pub struct BinOptions<'a> {
    filter: Option<&'a str>,
    output_ending: &'a str,
    print_query: bool,
    print_cmd: bool,
}

pub fn filter(
    bin_option: &BinOptions,
    options: &SkimOptions,
    source: Option<SkimItemReceiver>,
) -> Result<i32, std::io::Error> {
    let mut stdout = std::io::stdout();

    let default_command = match env::var("SKIM_DEFAULT_COMMAND").as_ref().map(String::as_ref) {
        Ok("") | Err(_) => "find .".to_owned(),
        Ok(val) => val.to_owned(),
    };
    let query = bin_option.filter.unwrap_or("");
    let cmd = options.cmd.unwrap_or(&default_command);

    // output query
    if bin_option.print_query {
        write!(stdout, "{}{}", query, bin_option.output_ending)?;
    }

    if bin_option.print_cmd {
        write!(stdout, "{}{}", cmd, bin_option.output_ending)?;
    }

    //------------------------------------------------------------------------------
    // matcher
    let engine_factory: Box<dyn MatchEngineFactory> = if options.regex {
        Box::new(RegexEngineFactory::builder())
    } else {
        let fuzzy_engine_factory = ExactOrFuzzyEngineFactory::builder()
            .fuzzy_algorithm(options.algorithm)
            .exact_mode(options.exact)
            .build();
        Box::new(AndOrEngineFactory::new(fuzzy_engine_factory))
    };

    let engine = engine_factory.create_engine_with_case(query, options.case);

    //------------------------------------------------------------------------------
    // start
    let components_to_stop = Arc::new(AtomicUsize::new(0));

    let stream_of_item = source.unwrap_or_else(|| {
        let cmd_collector = options.cmd_collector.clone();
        let (ret, _control) = cmd_collector.borrow_mut().invoke(cmd, components_to_stop);
        ret
    });

    let mut num_matched = 0;
    stream_of_item
        .into_iter()
        .filter_map(|item| engine.match_item(item.clone()).map(|result| (item, result)))
        .try_for_each(|(item, _match_result)| {
            num_matched += 1;
            write!(stdout, "{}{}", item.output(), bin_option.output_ending)
        })?;

    Ok(if num_matched == 0 { 1 } else { 0 })
}

fn stdin_autoheight_reader(options: &mut SkimOptions) -> Option<impl BufRead> {
    if !options.auto_height {
        return None;
    }

    let height = match options.height {
        TermHeight::Fixed(h) => std::cmp::max(h, 3),
        TermHeight::Percent(p) => {
            let th = get_terminal_height().ok()?;
            std::cmp::max((th * p) / 100, 3)
        }
    };

    let delim = if options.read0 { b'\0' } else { b'\n' };
    let stdin = io::stdin();
    let mut stdin_lock = stdin.lock();

    let mut buf = Vec::new();
    let mut lines_read = 0;

    while lines_read < height - 2 {
        match stdin_lock.read_until(delim, &mut buf) {
            Ok(0) | Err(_) => break,
            _ => lines_read += 1,
        }
    }

    let new_height = std::cmp::max(lines_read + 2, 3);
    options.height = TermHeight::Fixed(new_height);
    options.min_height = TermHeight::Fixed(new_height);

    drop(stdin_lock);
    Some(io::Cursor::new(buf).chain(BufReader::new(stdin)))
}

fn get_terminal_height() -> io::Result<usize> {
    use nix::libc::{ioctl, winsize, STDOUT_FILENO, TIOCGWINSZ};
    unsafe {
        // safety: winsize is just integers that can safely be zeroed
        let mut size: winsize = std::mem::zeroed();
        match ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut size) {
            0 => Ok(size.ws_row as usize),
            -1 => Err(io::Error::last_os_error()),
            ret => panic!("TIOCGWINSZ ioctl returned unexpected value {ret}"),
        }
    }
}

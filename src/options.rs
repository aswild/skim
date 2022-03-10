use std::num::ParseIntError;
use std::rc::Rc;
use std::str::FromStr;

use derive_builder::Builder;
use thiserror::Error;

use crate::helper::item_reader::SkimItemReader;
use crate::reader::CommandCollector;
use crate::{CaseMatching, FuzzyAlgorithm, MatchEngineFactory, Selector};
use std::cell::RefCell;

#[derive(Builder)]
#[builder(build_fn(name = "final_build"))]
#[builder(default)]
pub struct SkimOptions<'a> {
    pub bind: Vec<&'a str>,
    pub multi: bool,
    pub prompt: Option<&'a str>,
    pub cmd_prompt: Option<&'a str>,
    pub expect: Option<String>,
    pub tac: bool,
    pub nosort: bool,
    pub tiebreak: Option<String>,
    pub exact: bool,
    pub cmd: Option<&'a str>,
    pub interactive: bool,
    pub query: Option<&'a str>,
    pub cmd_query: Option<&'a str>,
    pub regex: bool,
    pub delimiter: Option<&'a str>,
    pub replstr: Option<&'a str>,
    pub color: Option<&'a str>,
    pub margin: Option<&'a str>,
    pub no_height: bool,
    pub no_clear: bool,
    pub min_height: TermHeight,
    pub height: TermHeight,
    pub auto_height: bool,
    pub preview: Option<&'a str>,
    pub preview_window: Option<&'a str>,
    pub reverse: bool,
    pub tabstop: Option<&'a str>,
    pub no_hscroll: bool,
    pub no_mouse: bool,
    pub inline_info: bool,
    pub header: Option<&'a str>,
    pub header_lines: usize,
    pub layout: &'a str,
    pub algorithm: FuzzyAlgorithm,
    pub case: CaseMatching,
    pub engine_factory: Option<Rc<dyn MatchEngineFactory>>,
    pub query_history: &'a [String],
    pub cmd_history: &'a [String],
    pub cmd_collector: Rc<RefCell<dyn CommandCollector>>,
    pub keep_right: bool,
    pub skip_to_pattern: &'a str,
    pub select1: bool,
    pub exit0: bool,
    pub sync: bool,
    pub selector: Option<Rc<dyn Selector>>,
    pub no_clear_if_empty: bool,
    pub read0: bool,
}

impl<'a> Default for SkimOptions<'a> {
    fn default() -> Self {
        Self {
            bind: vec![],
            multi: false,
            prompt: Some("> "),
            cmd_prompt: Some("c> "),
            expect: None,
            tac: false,
            nosort: false,
            tiebreak: None,
            exact: false,
            cmd: None,
            interactive: false,
            query: None,
            cmd_query: None,
            regex: false,
            delimiter: None,
            replstr: Some("{}"),
            color: None,
            margin: Some("0,0,0,0"),
            no_height: false,
            no_clear: false,
            min_height: TermHeight::Fixed(10),
            height: Default::default(),
            auto_height: false,
            preview: None,
            preview_window: Some("right:50%"),
            reverse: false,
            tabstop: None,
            no_hscroll: false,
            no_mouse: false,
            inline_info: false,
            header: None,
            header_lines: 0,
            layout: "",
            algorithm: FuzzyAlgorithm::default(),
            case: CaseMatching::default(),
            engine_factory: None,
            query_history: &[],
            cmd_history: &[],
            cmd_collector: Rc::new(RefCell::new(SkimItemReader::new(Default::default()))),
            keep_right: false,
            skip_to_pattern: "",
            select1: false,
            exit0: false,
            sync: false,
            selector: None,
            no_clear_if_empty: false,
            read0: false,
        }
    }
}

impl<'a> SkimOptionsBuilder<'a> {
    pub fn build(&mut self) -> SkimOptions<'a> {
        if let Some(true) = self.no_height {
            self.height = Some(TermHeight::Percent(100));
        }

        if let Some(true) = self.reverse {
            self.layout = Some("reverse");
        }

        // derive_builder could fail if required options aren't set, but this builder has default
        // values for everything so there's no way it can fail
        self.final_build().unwrap()
    }
}

/// Type to represent the terminal height, i.e. the parsed form of the --height and --min-height
/// options. This is currently a reimplementation of `[tuikit::term::TermHeight]`.
#[derive(Debug, Clone, Copy)]
pub enum TermHeight {
    Fixed(usize),
    Percent(usize),
}

impl Default for TermHeight {
    fn default() -> Self {
        Self::Percent(100)
    }
}

impl FromStr for TermHeight {
    type Err = TermHeightError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.strip_suffix('%') {
            Some(ps) => {
                let p = ps.parse::<usize>()?;
                if p <= 100 {
                    Ok(Self::Percent(p))
                } else {
                    Err(TermHeightError::PercentOutOfRange(p))
                }
            }
            None => Ok(Self::Fixed(s.parse::<usize>()?)),
        }
    }
}

impl From<TermHeight> for tuikit::term::TermHeight {
    fn from(height: TermHeight) -> Self {
        match height {
            TermHeight::Fixed(h) => Self::Fixed(h),
            TermHeight::Percent(h) => Self::Percent(h),
        }
    }
}

#[derive(Debug, Error)]
pub enum TermHeightError {
    #[error("Invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("Percentage is greater than 100%")]
    PercentOutOfRange(usize),
}

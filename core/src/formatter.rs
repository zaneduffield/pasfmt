use std::collections::HashSet;
use std::marker::PhantomData;

use crate::lang::*;
use crate::traits::*;

enum PostParseConsolidatorKind {
    Line(Box<dyn LogicalLinesConsolidator + Sync>),
    Token(Box<dyn TokenConsolidator + Sync>),
}
impl LogicalLinesConsolidator for PostParseConsolidatorKind {
    fn consolidate(&self, (tokens, lines): (&mut [Token], &mut [LogicalLine])) {
        match self {
            PostParseConsolidatorKind::Line(consolidator) => {
                consolidator.consolidate((tokens, lines))
            }
            PostParseConsolidatorKind::Token(consolidator) => consolidator.consolidate(tokens),
        }
    }
}

pub struct Formatter {
    lexer: Box<dyn Lexer + Sync>,
    token_consolidators: Vec<Box<dyn TokenConsolidator + Sync>>,
    logical_line_parser: Box<dyn LogicalLineParser + Sync>,
    post_parse_consolidators: Vec<PostParseConsolidatorKind>,
    token_removers: Vec<Box<dyn TokenRemover + Sync>>,
    logical_line_formatters: Vec<FormatterKind>,
    reconstructor: Box<dyn LogicalLinesReconstructor + Sync>,
}
impl Formatter {
    pub fn builder() -> FormatterBuilder<WithNothing> {
        FormatterBuilder::default()
    }
    pub fn format(&self, input: &str) -> String {
        let mut tokens = self.lexer.lex(input);
        for token_consolidator in self.token_consolidators.iter() {
            token_consolidator.consolidate(&mut tokens);
        }
        let mut lines = self.logical_line_parser.parse(&tokens);
        for line_consolidator in self.post_parse_consolidators.iter() {
            line_consolidator.consolidate((&mut tokens, &mut lines));
        }
        let mut tokens_marked_for_deletion: HashSet<usize> = HashSet::new();
        for token_remover in self.token_removers.iter() {
            token_remover.remove_tokens((&tokens, &lines), &mut tokens_marked_for_deletion);
        }
        delete_marked_tokens(tokens_marked_for_deletion, &mut tokens, &mut lines);
        delete_voided_logical_lines(&mut lines);

        let mut formatted_tokens = FormattedTokens::new_from_tokens(&tokens);
        for formatter in self.logical_line_formatters.iter() {
            formatter.format(&mut formatted_tokens, &lines);
        }
        self.reconstructor.reconstruct(formatted_tokens)
    }
}

fn delete_voided_logical_lines(lines: &mut Vec<LogicalLine>) {
    lines.retain(|line| line.get_line_type() != LogicalLineType::Voided);
}

fn delete_marked_tokens(
    marked_tokens: HashSet<usize>,
    tokens: &mut Vec<Token>,
    lines: &mut [LogicalLine],
) {
    if marked_tokens.is_empty() {
        return;
    }

    let mut new_indices: Vec<usize> = Vec::with_capacity(tokens.len());
    let mut current_index = 0;

    for token in tokens.iter() {
        new_indices.push(current_index);
        if !marked_tokens.contains(&token.get_index()) {
            current_index += 1;
        }
    }
    tokens.retain(|token| !marked_tokens.contains(&token.get_index()));

    for line in lines.iter_mut() {
        let tokens = line.get_tokens_mut();

        *tokens = tokens
            .iter()
            .filter_map(|token_index| match marked_tokens.contains(token_index) {
                false => Some(new_indices[*token_index]),
                true => None,
            })
            .collect();

        if tokens.is_empty() {
            line.void_and_drain();
        }
    }
}

macro_rules! builder_state {
    ($name: ident $(: [ $($trait_name: ident),* ] )?) => {
        #[derive(Default)]
        pub struct $name;
        $(
            $(
                impl $trait_name for $name {}
            )*
        )?
    };
}

trait CanAddLexer {}
trait CanAddTokenConsolidator {}
trait CanAddParser {}
trait CanAddPostParseConsolidator {}
trait CanAddTokenRemover {}
trait CanAddFormatter {}
trait CanAddReconstructor {}
trait CanBuild {}

builder_state!(WithNothing: [CanAddLexer]);
builder_state!(WithLexer: [CanAddTokenConsolidator, CanAddParser]);
builder_state!(WithTokenConsolidator: [CanAddTokenConsolidator, CanAddParser]);
builder_state!(WithParser: [CanAddPostParseConsolidator, CanAddTokenRemover, CanAddFormatter, CanAddReconstructor]);
builder_state!(WithLinesConsolidator: [CanAddPostParseConsolidator, CanAddTokenRemover, CanAddFormatter, CanAddReconstructor]);
builder_state!(WithTokenRemover: [CanAddTokenRemover, CanAddFormatter, CanAddReconstructor]);
builder_state!(WithFormatter: [CanAddFormatter, CanAddReconstructor]);
builder_state!(WithReconstructor: [CanBuild]);

trait FormattingBuilderData: Sized {
    fn set_lexer<L: Lexer + Sync + 'static>(self, lexer: L) -> Self;
    fn add_token_consolidator<T: TokenConsolidator + Sync + 'static>(
        self,
        token_consolidator: T,
    ) -> Self;
    fn set_line_parser<T: LogicalLineParser + Sync + 'static>(self, line_parser: T) -> Self;
    fn add_post_parse_consolidator(self, lines_consolidator: PostParseConsolidatorKind) -> Self;
    fn add_token_remover<T: TokenRemover + Sync + 'static>(self, token_remover: T) -> Self;
    fn add_formatter(self, logical_line_formatter: FormatterKind) -> Self;
    fn set_reconstructor<T: LogicalLinesReconstructor + Sync + 'static>(
        self,
        reconstructor: T,
    ) -> Self;

    fn convert_type<U>(self) -> FormatterBuilder<U>;
}

pub trait AddLexer {
    fn lexer<T: Lexer + Sync + 'static>(self, lexer: T) -> FormatterBuilder<WithLexer>;
}
pub trait AddTokenConsolidator {
    fn token_consolidator<T: TokenConsolidator + Sync + 'static>(
        self,
        token_consolidator: T,
    ) -> FormatterBuilder<WithTokenConsolidator>;
}
pub trait AddParser {
    fn parser<T: LogicalLineParser + Sync + 'static>(
        self,
        parser: T,
    ) -> FormatterBuilder<WithParser>;
}
pub trait AddPostParseConsolidator {
    fn token_consolidator<T: TokenConsolidator + Sync + 'static>(
        self,
        token_consolidator: T,
    ) -> FormatterBuilder<WithLinesConsolidator>;
    fn lines_consolidator<T: LogicalLinesConsolidator + Sync + 'static>(
        self,
        lines_consolidator: T,
    ) -> FormatterBuilder<WithLinesConsolidator>;
}
pub trait AddTokenRemover {
    fn token_remover<T: TokenRemover + Sync + 'static>(
        self,
        token_remover: T,
    ) -> FormatterBuilder<WithTokenRemover>;
}
pub trait AddFormatter {
    fn line_formatter<T: LogicalLineFormatter + Sync + 'static>(
        self,
        formatter: T,
    ) -> FormatterBuilder<WithFormatter>;
    fn file_formatter<T: LogicalLineFileFormatter + Sync + 'static>(
        self,
        formatter: T,
    ) -> FormatterBuilder<WithFormatter>;
}
pub trait AddReconstructor {
    fn reconstructor<T: LogicalLinesReconstructor + Sync + 'static>(
        self,
        reconstructor: T,
    ) -> FormatterBuilder<WithReconstructor>;
}
pub trait BuildFormatter {
    fn build(self) -> Formatter;
}

#[derive(Default)]
pub struct FormatterBuilder<T> {
    lexer: Option<Box<dyn Lexer + Sync>>,
    token_consolidators: Vec<Box<dyn TokenConsolidator + Sync + 'static>>,
    logical_line_parser: Option<Box<dyn LogicalLineParser + Sync + 'static>>,
    post_parse_consolidators: Vec<PostParseConsolidatorKind>,
    token_removers: Vec<Box<dyn TokenRemover + Sync + 'static>>,
    logical_line_formatters: Vec<FormatterKind>,
    reconstructor: Option<Box<dyn LogicalLinesReconstructor + Sync + 'static>>,
    builder_state: PhantomData<T>,
}
impl<T> FormatterBuilder<T> {
    fn convert_type<U>(self) -> FormatterBuilder<U> {
        FormatterBuilder::<U> {
            lexer: self.lexer,
            token_consolidators: self.token_consolidators,
            logical_line_parser: self.logical_line_parser,
            post_parse_consolidators: self.post_parse_consolidators,
            token_removers: self.token_removers,
            logical_line_formatters: self.logical_line_formatters,
            reconstructor: self.reconstructor,
            builder_state: PhantomData,
        }
    }
}
impl<T> FormattingBuilderData for FormatterBuilder<T> {
    fn set_lexer<L: Lexer + Sync + 'static>(mut self, lexer: L) -> Self {
        self.lexer = Some(Box::new(lexer));
        self
    }
    fn add_token_consolidator<C: TokenConsolidator + Sync + 'static>(
        mut self,
        token_consolidator: C,
    ) -> Self {
        self.token_consolidators.push(Box::new(token_consolidator));
        self
    }
    fn set_line_parser<P: LogicalLineParser + Sync + 'static>(mut self, line_parser: P) -> Self {
        self.logical_line_parser = Some(Box::new(line_parser));
        self
    }
    fn add_post_parse_consolidator(
        mut self,
        post_parse_consolidator: PostParseConsolidatorKind,
    ) -> Self {
        self.post_parse_consolidators.push(post_parse_consolidator);
        self
    }
    fn add_token_remover<R: TokenRemover + Sync + 'static>(mut self, token_remover: R) -> Self {
        self.token_removers.push(Box::new(token_remover));
        self
    }
    fn add_formatter(mut self, logical_line_formatter: FormatterKind) -> Self {
        self.logical_line_formatters.push(logical_line_formatter);
        self
    }
    fn set_reconstructor<R: LogicalLinesReconstructor + Sync + 'static>(
        mut self,
        reconstructor: R,
    ) -> Self {
        self.reconstructor = Some(Box::new(reconstructor));
        self
    }

    fn convert_type<U>(self) -> FormatterBuilder<U> {
        FormatterBuilder::<U> {
            lexer: self.lexer,
            token_consolidators: self.token_consolidators,
            logical_line_parser: self.logical_line_parser,
            post_parse_consolidators: self.post_parse_consolidators,
            token_removers: self.token_removers,
            logical_line_formatters: self.logical_line_formatters,
            reconstructor: self.reconstructor,
            builder_state: PhantomData,
        }
    }
}

impl<U: CanAddLexer> AddLexer for FormatterBuilder<U> {
    fn lexer<T: Lexer + Sync + 'static>(self, lexer: T) -> FormatterBuilder<WithLexer> {
        self.set_lexer(lexer).convert_type()
    }
}
impl<U: CanAddTokenConsolidator> AddTokenConsolidator for FormatterBuilder<U> {
    fn token_consolidator<T: TokenConsolidator + Sync + 'static>(
        self,
        token_consolidator: T,
    ) -> FormatterBuilder<WithTokenConsolidator> {
        self.add_token_consolidator(token_consolidator)
            .convert_type()
    }
}
impl<U: CanAddParser> AddParser for FormatterBuilder<U> {
    fn parser<T: LogicalLineParser + Sync + 'static>(
        self,
        parser: T,
    ) -> FormatterBuilder<WithParser> {
        self.set_line_parser(parser).convert_type()
    }
}
impl<U: CanAddPostParseConsolidator> AddPostParseConsolidator for FormatterBuilder<U> {
    fn token_consolidator<T: TokenConsolidator + Sync + 'static>(
        self,
        token_consolidator: T,
    ) -> FormatterBuilder<WithLinesConsolidator> {
        self.add_post_parse_consolidator(PostParseConsolidatorKind::Token(Box::new(
            token_consolidator,
        )))
        .convert_type()
    }
    fn lines_consolidator<T: LogicalLinesConsolidator + Sync + 'static>(
        self,
        lines_consolidator: T,
    ) -> FormatterBuilder<WithLinesConsolidator> {
        self.add_post_parse_consolidator(PostParseConsolidatorKind::Line(Box::new(
            lines_consolidator,
        )))
        .convert_type()
    }
}
impl<U: CanAddTokenRemover> AddTokenRemover for FormatterBuilder<U> {
    fn token_remover<T: TokenRemover + Sync + 'static>(
        self,
        token_remover: T,
    ) -> FormatterBuilder<WithTokenRemover> {
        self.add_token_remover(token_remover).convert_type()
    }
}
impl<U: CanAddFormatter> AddFormatter for FormatterBuilder<U> {
    fn line_formatter<T: LogicalLineFormatter + Sync + 'static>(
        self,
        formatter: T,
    ) -> FormatterBuilder<WithFormatter> {
        self.add_formatter(FormatterKind::LineFormatter(Box::new(formatter)))
            .convert_type()
    }

    fn file_formatter<T: LogicalLineFileFormatter + Sync + 'static>(
        self,
        formatter: T,
    ) -> FormatterBuilder<WithFormatter> {
        self.add_formatter(FormatterKind::FileFormatter(Box::new(formatter)))
            .convert_type()
    }
}
impl<U: CanAddReconstructor> AddReconstructor for FormatterBuilder<U> {
    fn reconstructor<T: LogicalLinesReconstructor + Sync + 'static>(
        self,
        reconstructor: T,
    ) -> FormatterBuilder<WithReconstructor> {
        self.set_reconstructor(reconstructor).convert_type()
    }
}
impl<U: CanBuild> BuildFormatter for FormatterBuilder<U> {
    fn build(self) -> Formatter {
        Formatter {
            lexer: self.lexer.unwrap(),
            token_consolidators: self.token_consolidators,
            logical_line_parser: self.logical_line_parser.unwrap(),
            post_parse_consolidators: self.post_parse_consolidators,
            token_removers: self.token_removers,
            logical_line_formatters: self.logical_line_formatters,
            reconstructor: self.reconstructor.unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::*;
    use indoc::indoc;
    use itertools::Itertools;
    use spectral::prelude::*;

    fn run_test(formatter: Formatter, input: &str, expected_output: &str) {
        let output = formatter.format(input);
        assert_that(&output).is_equal_to(expected_output.to_string());
    }

    fn create_token_with_index<'a>(index: usize) -> Token<'a> {
        Token::OwningToken(OwningToken::new(
            index,
            "".to_owned(),
            "".to_owned(),
            TokenType::Unknown,
        ))
    }

    fn run_token_deletion_test(
        tokens: Vec<usize>,
        marked_tokens: Vec<usize>,
        lines: Vec<Vec<usize>>,
        expected_lines: Vec<Vec<usize>>,
    ) {
        let mut tokens = tokens
            .iter()
            .map(|&index| create_token_with_index(index))
            .collect_vec();
        let marked_tokens: HashSet<usize> = marked_tokens.into_iter().collect();
        let mut lines = lines
            .into_iter()
            .map(|line_indices| LogicalLine::new(None, 0, line_indices, LogicalLineType::Unknown))
            .collect_vec();
        let expected_lines = expected_lines
            .into_iter()
            .map(|line_indices| LogicalLine::new(None, 0, line_indices, LogicalLineType::Unknown))
            .collect_vec();

        delete_marked_tokens(marked_tokens, &mut tokens, &mut lines);
        delete_voided_logical_lines(&mut lines);
        assert_that(&lines).equals_iterator(&expected_lines.iter());
    }

    #[test]
    fn delete_first_token() {
        run_token_deletion_test(
            vec![0, 1, 2],
            vec![0],
            vec![vec![0, 1, 2], vec![0, 2], vec![1, 2], vec![0, 1]],
            vec![vec![0, 1], vec![1], vec![0, 1], vec![0]],
        );
    }

    #[test]
    fn delete_last_token() {
        run_token_deletion_test(
            vec![0, 1, 2],
            vec![2],
            vec![vec![0, 1, 2], vec![0, 2], vec![0, 1], vec![0, 2]],
            vec![vec![0, 1], vec![0], vec![0, 1], vec![0]],
        );
    }

    #[test]
    fn delete_middle_token() {
        run_token_deletion_test(
            vec![0, 1, 2],
            vec![1],
            vec![vec![0, 1, 2], vec![0, 2], vec![0, 1], vec![0, 2]],
            vec![vec![0, 1], vec![0, 1], vec![0], vec![0, 1]],
        );
    }

    #[test]
    fn delete_middle_tokens() {
        run_token_deletion_test(
            vec![0, 1, 2, 3],
            vec![1, 2],
            vec![
                vec![0, 1, 2, 3],
                vec![0, 3],
                vec![1, 2],
                vec![0, 1, 3],
                vec![0, 2, 3],
                vec![1, 3],
                vec![2, 3],
            ],
            vec![
                vec![0, 1],
                vec![0, 1],
                vec![0, 1],
                vec![0, 1],
                vec![1],
                vec![1],
            ],
        );
    }

    struct RemoveAllParens;
    impl TokenRemover for RemoveAllParens {
        fn remove_tokens(
            &self,
            (tokens, _): (&[Token], &[LogicalLine]),
            marked_tokens: &mut HashSet<usize>,
        ) {
            for token in tokens {
                if matches!(
                    token.get_token_type(),
                    TokenType::Op(OperatorKind::LParen | OperatorKind::RParen)
                ) {
                    marked_tokens.insert(token.get_index());
                }
            }
        }
    }

    struct RemoveAllAsteriscs;
    impl TokenRemover for RemoveAllAsteriscs {
        fn remove_tokens(
            &self,
            (tokens, _): (&[Token], &[LogicalLine]),
            marked_tokens: &mut HashSet<usize>,
        ) {
            for token in tokens {
                if token.get_content().eq_ignore_ascii_case("*") {
                    marked_tokens.insert(token.get_index());
                }
            }
        }
    }

    #[test]
    fn one_token_remover() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .token_remover(RemoveAllParens {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "(a)", "a");
    }

    #[test]
    fn two_token_removers() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .token_remover(RemoveAllParens {})
            .token_remover(RemoveAllAsteriscs {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "([*a*])", "[a]");
    }

    struct MakeMultiplySignIdentifier;
    impl TokenConsolidator for MakeMultiplySignIdentifier {
        fn consolidate(&self, tokens: &mut [Token]) {
            for token in tokens.iter_mut() {
                if token.get_token_type() == TokenType::Op(OperatorKind::Star) {
                    token.set_token_type(TokenType::Identifier);
                }
            }
        }
    }
    struct MakeFirstMultiplySignIdentifierInFirstLine;
    impl LogicalLinesConsolidator for MakeFirstMultiplySignIdentifierInFirstLine {
        fn consolidate(&self, (tokens, lines): (&mut [Token], &mut [LogicalLine])) {
            if let Some(&first_mul) = lines.first().and_then(|line| {
                line.get_tokens().iter().find(|&&token| {
                    tokens.get(token).unwrap().get_token_type() == TokenType::Op(OperatorKind::Star)
                })
            }) {
                tokens[first_mul].set_token_type(TokenType::Identifier);
            }
        }
    }

    struct Append1ToAllIdentifiers;
    impl TokenConsolidator for Append1ToAllIdentifiers {
        fn consolidate(&self, tokens: &mut [Token]) {
            (0..tokens.len() - 1).for_each(|token_index| {
                if let Some(token) = tokens.get_mut(token_index) {
                    if token.get_token_type() != TokenType::Identifier {
                        return;
                    }
                    *token = Token::OwningToken(OwningToken::new(
                        token.get_index(),
                        token.get_leading_whitespace().to_owned(),
                        token.get_content().to_owned() + "1",
                        token.get_token_type(),
                    ));
                }
            });
        }
    }

    struct AddSpaceBeforeIdentifier;
    impl LogicalLineFormatter for AddSpaceBeforeIdentifier {
        fn format(&self, formatted_tokens: &mut FormattedTokens<'_>, input: &LogicalLine) {
            for &token in input.get_tokens() {
                if formatted_tokens.get_token_type_for_index(token) == Some(TokenType::Identifier) {
                    if let Some(formatting_data) = formatted_tokens.get_formatting_data_mut(token) {
                        formatting_data.spaces_before = 1;
                    }
                }
            }
        }
    }

    #[test]
    fn no_optional_stages() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "a 1 b 2 c 3", "a 1 b 2 c 3");
    }

    #[test]
    fn single_token_consolidator_before_parsing() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .token_consolidator(MakeMultiplySignIdentifier {})
            .parser(DelphiLogicalLineParser {})
            .line_formatter(AddSpaceBeforeIdentifier {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "1*1*1", "1 *1 *1");
    }

    #[test]
    fn multiple_token_consolidators_before_parsing() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .token_consolidator(MakeMultiplySignIdentifier {})
            .token_consolidator(Append1ToAllIdentifiers {})
            .parser(DelphiLogicalLineParser {})
            .line_formatter(AddSpaceBeforeIdentifier {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "1*1*1", "1 *11 *11");
    }

    #[test]
    fn single_token_consolidator_after_parsing() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .token_consolidator(MakeMultiplySignIdentifier {})
            .line_formatter(AddSpaceBeforeIdentifier {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "1*1*1", "1 *1 *1");
    }

    #[test]
    fn multiple_token_consolidators_after_parsing() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .token_consolidator(MakeMultiplySignIdentifier {})
            .token_consolidator(Append1ToAllIdentifiers {})
            .line_formatter(AddSpaceBeforeIdentifier {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "1*1*1", "1 *11 *11");
    }

    struct BreakBeforeMultiply;
    impl LogicalLineFormatter for BreakBeforeMultiply {
        fn format(&self, formatted_tokens: &mut FormattedTokens<'_>, input: &LogicalLine) {
            for &token in input.get_tokens() {
                if formatted_tokens.get_token_type_for_index(token)
                    == Some(TokenType::Op(OperatorKind::Star))
                {
                    if let Some(formatting_data) = formatted_tokens.get_formatting_data_mut(token) {
                        formatting_data.spaces_before = 0;
                        formatting_data.newlines_before = 1;
                    }
                }
            }
        }
    }

    #[test]
    fn line_then_token_consolidators_after_parsing() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .lines_consolidator(MakeFirstMultiplySignIdentifierInFirstLine {})
            .token_consolidator(Append1ToAllIdentifiers {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "A*B*C;D*E", "A1*1B1*C1;D1*E1");
    }

    #[test]
    fn token_then_line_consolidators_after_parsing() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .token_consolidator(Append1ToAllIdentifiers {})
            .lines_consolidator(MakeFirstMultiplySignIdentifierInFirstLine {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(formatter, "A*B*C;D*E", "A1*B1*C1;D1*E1");
    }

    struct CombineFirst2Lines;
    impl LogicalLinesConsolidator for CombineFirst2Lines {
        fn consolidate(&self, (_, lines): (&mut [Token<'_>], &mut [LogicalLine])) {
            let (second_non_void_line_index, _) = match lines
                .iter()
                .enumerate()
                .filter(|(_, line)| line.get_line_type() != LogicalLineType::Voided)
                .take(2)
                .last()
            {
                Some(line) => line,
                _ => return,
            };

            let second_line_tokens = match lines.get_mut(second_non_void_line_index) {
                Some(line) => line.void_and_drain().collect_vec(),
                _ => return,
            };

            lines
                .get_mut(0)
                .unwrap()
                .get_tokens_mut()
                .extend(second_line_tokens);
        }
    }

    struct LogicalLinesOnNewLines;
    impl LogicalLineFormatter for LogicalLinesOnNewLines {
        fn format(&self, formatted_tokens: &mut FormattedTokens<'_>, input: &LogicalLine) {
            if let Some(&first_token) = input.get_tokens().first() {
                if first_token != 0 && first_token != formatted_tokens.get_tokens().len() - 1 {
                    if let Some(formatting_data) =
                        formatted_tokens.get_formatting_data_mut(first_token)
                    {
                        formatting_data.spaces_before = 0;
                        formatting_data.newlines_before = 1;
                    }
                }
            }
        }
    }

    #[test]
    fn single_line_consolidator() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .token_consolidator(MakeMultiplySignIdentifier {})
            .parser(DelphiLogicalLineParser {})
            .lines_consolidator(CombineFirst2Lines {})
            .line_formatter(LogicalLinesOnNewLines {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(
            formatter,
            "a; b; c; d;",
            indoc! {"
                a; b;
                c;
                d;"
            },
        );
    }

    #[test]
    fn multiple_line_consolidators() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .lines_consolidator(CombineFirst2Lines {})
            .lines_consolidator(CombineFirst2Lines {})
            .line_formatter(LogicalLinesOnNewLines {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(
            formatter,
            "a; b; c; d;",
            indoc! {"
                a; b; c;
                d;"
            },
        );
    }

    struct SpaceBeforeSemiColon;
    impl LogicalLineFormatter for SpaceBeforeSemiColon {
        fn format(&self, formatted_tokens: &mut FormattedTokens<'_>, input: &LogicalLine) {
            let semicolon_indices: Vec<_> = input
                .get_tokens()
                .iter()
                .filter(|&&token_index| {
                    formatted_tokens.get_token_type_for_index(token_index)
                        == Some(TokenType::Op(OperatorKind::Semicolon))
                })
                .collect();

            semicolon_indices.iter().for_each(|&&semicolon_index| {
                if let Some(semicolon_formatting_data) =
                    formatted_tokens.get_formatting_data_mut(semicolon_index)
                {
                    semicolon_formatting_data.spaces_before = 1;
                }
            });
        }
    }

    #[test]
    fn single_line_formatter() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .line_formatter(LogicalLinesOnNewLines {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(
            formatter,
            "a; b; c; d;",
            indoc! {"
                a;
                b;
                c;
                d;"
            },
        );
    }
    #[test]
    fn multiple_line_formatters() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .token_consolidator(MakeMultiplySignIdentifier {})
            .parser(DelphiLogicalLineParser {})
            .line_formatter(LogicalLinesOnNewLines {})
            .line_formatter(SpaceBeforeSemiColon {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(
            formatter,
            "a; b; c; d;",
            indoc! {"
                a ;
                b ;
                c ;
                d ;"
            },
        );
    }

    struct IndentBasedOnLineNumber;
    impl LogicalLineFileFormatter for IndentBasedOnLineNumber {
        fn format(&self, formatted_tokens: &mut FormattedTokens<'_>, input: &[LogicalLine]) {
            for (index, line) in input
                .iter()
                .enumerate()
                .filter(|(_, line)| !matches!(line.get_line_type(), LogicalLineType::Eof))
            {
                let &first_line_token = line.get_tokens().first().unwrap();
                let formatting_data = formatted_tokens
                    .get_formatting_data_mut(first_line_token)
                    .unwrap();
                formatting_data.spaces_before = 0;
                formatting_data.indentations_before = index;
            }
        }
    }

    #[test]
    fn single_file_formatter() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .file_formatter(IndentBasedOnLineNumber {})
            .reconstructor(DelphiLogicalLinesReconstructor::new(
                ReconstructionSettings::new("\n".to_owned(), " ".to_owned(), " ".to_owned()),
            ))
            .build();
        run_test(
            formatter,
            "a; b; c; d;",
            indoc! {"
                a; b;  c;   d;"
            },
        );
    }

    struct IndentSecondLine3SpacesIfNoNewLine;
    impl LogicalLineFileFormatter for IndentSecondLine3SpacesIfNoNewLine {
        fn format(&self, formatted_tokens: &mut FormattedTokens<'_>, input: &[LogicalLine]) {
            let &first_line_token = input[1].get_tokens().first().unwrap();
            let formatting_data = formatted_tokens
                .get_formatting_data_mut(first_line_token)
                .unwrap();
            if formatting_data.newlines_before == 0 {
                formatting_data.spaces_before = 3;
            }
            formatting_data.indentations_before = 0;
        }
    }

    #[test]
    fn multiple_file_formatter() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .token_consolidator(MakeMultiplySignIdentifier {})
            .parser(DelphiLogicalLineParser {})
            .file_formatter(IndentBasedOnLineNumber {})
            .file_formatter(IndentSecondLine3SpacesIfNoNewLine {})
            .reconstructor(DelphiLogicalLinesReconstructor::new(
                ReconstructionSettings::new("\n".to_owned(), " ".to_owned(), " ".to_owned()),
            ))
            .build();
        run_test(
            formatter,
            "a; b; c; d;",
            indoc! {"
                a;   b;  c;   d;"
            },
        );
    }

    struct RetainSpacesLogcialLinesOnNewLines;
    impl LogicalLineFormatter for RetainSpacesLogcialLinesOnNewLines {
        fn format(&self, formatted_tokens: &mut FormattedTokens<'_>, input: &LogicalLine) {
            let first_token = *input.get_tokens().first().unwrap();
            if first_token != 0 && first_token != formatted_tokens.get_tokens().len() - 1 {
                if let Some(formatting_data) = formatted_tokens.get_formatting_data_mut(first_token)
                {
                    formatting_data.newlines_before = 1;
                }
            }
        }
    }

    #[test]
    fn line_then_file_formatter() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .line_formatter(RetainSpacesLogcialLinesOnNewLines {})
            .file_formatter(IndentSecondLine3SpacesIfNoNewLine {})
            .reconstructor(default_test_reconstructor())
            .build();
        run_test(
            formatter,
            "a;b;c;d;",
            indoc! {"
                a;
                b;
                c;
                d;"
            },
        );
    }
    #[test]
    fn file_then_line_formatter() {
        let formatter = Formatter::builder()
            .lexer(DelphiLexer {})
            .parser(DelphiLogicalLineParser {})
            .file_formatter(IndentSecondLine3SpacesIfNoNewLine {})
            .line_formatter(RetainSpacesLogcialLinesOnNewLines {})
            .reconstructor(DelphiLogicalLinesReconstructor::new(
                ReconstructionSettings::new("\n".to_owned(), " ".to_owned(), " ".to_owned()),
            ))
            .build();
        run_test(
            formatter,
            "a;b;c;d;",
            indoc! {"
                a;
                   b;
                c;
                d;"
            },
        );
    }
}

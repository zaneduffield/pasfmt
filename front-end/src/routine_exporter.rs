use pasfmt_core::{
    lang::{KeywordKind, LogicalLine, LogicalLineType, OperatorKind, Token, TokenData, TokenType},
    traits::LogicalLinesConsolidator,
};

pub struct RoutineExporter {}
impl LogicalLinesConsolidator for RoutineExporter {
    fn consolidate(
        &self,
        (tokens, lines): (&mut [pasfmt_core::prelude::Token], &mut [LogicalLine]),
    ) {
        if !tokens
            .iter()
            .any(|token| token.get_token_type() == TokenType::Keyword(KeywordKind::Unit))
        {
            return;
        }
        for line in lines {
            let line_tokens: Vec<_> = line
                .get_tokens()
                .iter()
                .flat_map(|&index| tokens.get(index))
                .collect();

            if line_tokens.iter().any(|token| {
                matches!(
                    token.get_token_type(),
                    TokenType::Keyword(KeywordKind::Implementation)
                )
            }) {
                return;
            }
            if !matches!(
                (line.get_line_type(), line.get_level()),
                (LogicalLineType::RoutineHeader, 0)
            ) {
                continue;
            }

            let param_list = if line_tokens.iter().any(|token| {
                matches!(
                    token.get_token_type(),
                    TokenType::Keyword(KeywordKind::Overload)
                )
            }) {
                let mut buf = String::new();
                buf.push('(');
                let mut paren_level = 0;
                for token in &line_tokens {
                    match token.get_token_type() {
                        TokenType::Op(OperatorKind::LParen) => paren_level += 1,
                        TokenType::Op(OperatorKind::RParen) => {
                            paren_level -= 1;
                            if paren_level == 0 {
                                break;
                            }
                        }
                        TokenType::Comment(_) => {}
                        _ if paren_level > 0 => {
                            buf.push(' ');
                            buf.push_str(token.get_content());
                        }
                        _ => {}
                    }
                }
                buf.push(')');
                buf
            } else {
                "".to_string()
            };
            let Some(ident_token) = line_tokens
                .iter()
                .find(|ctx| ctx.get_token_type() == TokenType::Identifier)
            else {
                continue;
            };

            let name = ident_token.get_content();
            let last_token = line_tokens.last().unwrap();
            let new_content = format!(
                "{}{} exports {}{};",
                last_token.get_leading_whitespace(),
                last_token.get_content(),
                name,
                param_list
            );
            let &last_index = line.get_tokens().last().unwrap();
            tokens[last_index] = Token::new_owned(
                new_content,
                last_token.get_leading_whitespace().len() as u32,
                last_token.get_token_type(),
            );
        }
    }
}

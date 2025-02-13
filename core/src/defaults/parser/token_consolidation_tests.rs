use std::str::FromStr;

use indoc::indoc;
use spectral::prelude::*;

use super::*;
use crate::lang::KeywordKindDiscriminants;
use crate::prelude::*;

#[derive(Debug, PartialEq, Eq)]
enum AssertionTokenType {
    Identifier,
    Keyword(KeywordKindDiscriminants),
}
impl From<TokenType> for AssertionTokenType {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Keyword(kk) => Self::Keyword(kk.into()),
            _ => Self::Identifier,
        }
    }
}

fn run_casing_token_consolidation_test(input: &str) {
    let lexer = &DelphiLexer {};
    let parser = &DelphiLogicalLineParser {};
    eprintln!("input: {input}");
    let tokens = lexer.lex(input);
    let (_, tokens, _) = parser.parse(tokens);

    let test_data = tokens
        .iter()
        .enumerate()
        .filter_map(|(index, token)| {
            match (
                KeywordKindDiscriminants::from_str(token.get_content()),
                token.get_token_type(),
            ) {
                (Ok(keyword_kind), TokenType::Keyword(_) | TokenType::Identifier) => Some((
                    index,
                    token.get_content(),
                    token.get_token_type(),
                    keyword_kind,
                )),
                _ => None,
            }
        })
        .collect_vec();
    let actual_kks: Vec<(usize, AssertionTokenType)> = test_data
        .iter()
        .map(|(index, _, token_type, _)| (*index, AssertionTokenType::from(*token_type)))
        .collect_vec();
    let expected_kks = test_data
        .iter()
        .map(|(index, token, _, expected_token_type)| {
            (
                *index,
                match token.chars().all(char::is_uppercase) {
                    true => AssertionTokenType::Identifier,
                    false => AssertionTokenType::Keyword(*expected_token_type),
                },
            )
        })
        .collect_vec();

    assert_that(&actual_kks).is_equal_to(expected_kks);
}

macro_rules! casing_token_consolidation_test {
    ($suite_name:ident, $($case_name: ident = { $input: expr $(,)? }),* $(,)?) => {
        #[yare::parameterized(
            $($case_name = { $input }),*
        )]
        fn $suite_name(input: &str) {
            run_casing_token_consolidation_test(input);
        }
    };
}

macro_rules! method_directive_consolidation_test {
    ($($case_name: ident = { $input: literal $(,)? }),* $(,)?) => {
        mod method_directives {
            use super::*;
            paste::paste! {
                $(
                    casing_token_consolidation_test!(
                        $case_name,
                        [<f_$case_name>] = { &format!("function foo: bar {};", $input) },
                        [<f_ $case_name _s>] = { &format!("function foo: bar; {};", $input) },
                        [<p_$case_name>] = { &format!("procedure foo {};", $input) },
                        [<p_ $case_name _s>] = { &format!("procedure foo; {};", $input) },
                        [<t_f_$case_name>] = { &format!("type tfoo = function foo: bar {};", $input) },
                        [<t_f_ $case_name _s>] = { &format!("type tfoo = function foo: bar; {};", $input) },
                        [<t_p_$case_name>] = { &format!("type tfoo = procedure foo {};", $input) },
                        [<t_p_ $case_name _s>] = { &format!("type tfoo = procedure foo; {};", $input) },
                    );
                )*
            }
        }
    };
}

macro_rules! method_directive_ident_consolidation_test {
    ($($case_name: ident = { $input: literal $(,)? }),* $(,)?) => {
        mod method_directive_idents {
            use super::*;
            paste::paste! {
                $(
                    casing_token_consolidation_test!(
                        $case_name,
                        [<$case_name _message>] = { &format!("function {}: {}; message {};", $input, $input, $input) },
                        [<$case_name _name>] = { &format!("function {}: {}; name {};", $input, $input, $input) },
                        [<$case_name _dispid>] = { &format!("function {}: {}; dispid {};", $input, $input, $input) },
                    );
                )*
            }
        }
    };
}

method_directive_consolidation_test!(
    forward = { "forward" },
    external = { "external" },
    external_dll = { "external dll_name" },
    external_name = { "external dll_name name dll_name" },
    external_name_directive_only = { "external name dll_name" },
    external_index = { "external dll_name index 1" },
    external_delayed = { "external dll_name delayed" },
    external_all = { "external dll_name name dll_name index 1 delayed" },
    overload = { "overload" },
    reintroduce = { "reintroduce" },
    message = { "message msg" },
    static_directive = { "static" },
    dynamic = { "dynamic" },
    override_directive = { "override" },
    virtual_directive = { "virtual" },
    abstract_directive = { "abstract" },
    final_directive = { "final" },
    inline = { "inline" },
    assembler = { "assembler" },
    cdecl = { "cdecl" },
    pascal = { "pascal" },
    register = { "register" },
    safecall = { "safecall" },
    stdcall = { "stdcall" },
    winapi = { "winapi" },
    export = { "export" },
    // Portability directives
    far = { "far" },
    local = { "local" },
    near = { "near" },
    dispid = { "dispid 1" },
    varargs = { "varargs" },
    unsafe_directive = { "unsafe" },
);

method_directive_ident_consolidation_test!(
    forward = { "FORWARD" },
    external = { "EXTERNAL" },
    name = { "NAME dll_name" },
    index = { "INDEX 1" },
    delayed = { "DELAYED" },
    overload = { "OVERLOAD" },
    reintroduce = { "REINTRODUCE" },
    message = { "MESSAGE msg" },
    static_directive = { "STATIC" },
    dynamic = { "DYNAMIC" },
    override_directive = { "OVERRIDE" },
    virtual_directive = { "VIRTUAL" },
    abstract_directive = { "ABSTRACT" },
    final_directive = { "FINAL" },
    // Inline is a pure keyword
    assembler = { "ASSEMBLER" },
    cdecl = { "CDECL" },
    pascal = { "PASCAL" },
    register = { "REGISTER" },
    safecall = { "SAFECALL" },
    stdcall = { "STDCALL" },
    winapi = { "WINAPI" },
    export = { "EXPORT" },
    // Portability directives
    far = { "FAR" },
    local = { "LOCAL" },
    near = { "NEAR" },
    dispid = { "DISPID 1" },
    varargs = { "VARARGS" },
    unsafe_directive = { "UNSAFE" },
);

casing_token_consolidation_test!(
    method_directive_extras,
    multiple = { "procedure foo; overload; deprecated 'bye bye'; dispid 123;" },
    t_multiple = { "type tfoo = procedure foo; overload; deprecated 'bye bye'; dispid 123;" },
    message_ident = { "procedure foo(MESSAGE: MESSAGE);" },
    identifier_after_proc = { "procedure REGISTER;" },
    identifier_after_colon = { "procedure INDEX: OVERLOAD;" },
    qualified_identifier = { "procedure READONLY.REGISTER;" },
    qualified_identifier_and_return = { "function READONLY.REGISTER: OVERLOAD;" },
);

casing_token_consolidation_test!(
    method_identifiers,
    function = { "type foo = class function READ: WRITE; end;" },
    procedure = { "type foo = class procedure READ; end;" },
    // These are `method resolution clauses`
    // https://docwiki.embarcadero.com/RADStudio/en/Implementing_Interfaces#Method_Resolution_Clause
    function_resolution = { "type foo = class function READ.WRITE = OPERATOR; end;" },
    procedure_resolution = { "type foo = class procedure READ.WRITE = OPERATOR; end;" },
);

casing_token_consolidation_test!(
    visibility,
    alone = {
        indoc! {"
            type
              tfoo = class
              private
              end"
        },
    },
    multiple = {
        indoc! {"
            type
              tfoo = class
              private
              protected
              public
              published
              automated
              end"
        },
    },
    strict = {
        indoc! {"
            type
              tfoo = class
              strict private
              strict protected
              end"
        },
    },
    misleading = {
        indoc! {"
            type
              tfoo = class
              protected
                property PUBLIC: integer read PRIVATE stored PROTECTED;
              automated
              end"
        },
    },
);

macro_rules! casing_property_declaration_consolidation_test(
    ($($case_name: ident = { $input: expr $(,)? }),* $(,)?) => {
        #[yare::parameterized(
            $($case_name = { &format!("type foo = class {} end", $input) }),*
        )]
        fn property_declaration(input: &str) {
            run_casing_token_consolidation_test(input);
        }
    };
);

casing_property_declaration_consolidation_test!(
    default = { "property foo: integer; default" },
    default_s = { "property foo: integer; default;" },
    default_expression = { "property foo: integer default foo" },
    default_expression_s = { "property foo: integer default foo;" },
    default_ident = { "property DEFAULT: DEFAULT read DEFAULT index DEFAULT + DEFAULT;" },
    read = { "property foo: integer read ffoo" },
    read_s = { "property foo: integer read ffoo;" },
    read_ident = { "property READ: READ read READ index READ + READ;" },
    write = { "property foo: integer write ffoo" },
    write_s = { "property foo: integer write ffoo;" },
    write_ident = { "property WRITE: WRITE index WRITE + WRITE;" },
    read_write = { "property foo: integer read ffoo write ffoo;" },
    readonly = { "property foo: integer readonly" },
    readonly_s = { "property foo: integer readonly;" },
    readonly_ident = { "property READONLY: READONLY read READONLY index READONLY + READONLY;" },
    writeonly = { "property foo: integer writeonly" },
    writeonly_s = { "property foo: integer writeonly;" },
    writeonly_ident =
        { "property WRITEONLY: WRITEONLY read WRITEONLY index WRITEONLY + WRITEONLY;" },
    dispid = { "property foo: integer dispid 1" },
    dispid_s = { "property foo: integer dispid 1;" },
    dispid_ident = { "property DISPID: DISPID dispid DISPID + DISPID;" },
    implements = { "property foo: integer implements ifoo" },
    implements_s = { "property foo: integer implements ifoo;" },
    implements_many = { "property foo: integer implements ifoo,ibar;" },
    implements_ident = {
        "property IMPLEMENTS: IMPLEMENTS implements IMPLEMENTS, IMPLEMENTS index IMPLEMENTS + IMPLEMENTS;"
    },
    index = { "property foo: integer index 1" },
    index_s = { "property foo: integer index 1;" },
    index_ident = { "property INDEX: INDEX index INDEX + INDEX;" },
    nodefault = { "property foo: integer nodefault" },
    nodefault_s = { "property foo: integer nodefault;" },
    nodefault_ident =
        { "property NODEFAULT: NODEFAULT read NODEFAULT index NODEFAULT + NODEFAULT;" },
    stored = { "property foo: integer stored true" },
    stored_s = { "property foo: integer stored true;" },
    stored_ident = { "property STORED: STORED stored STORED and STORED;" },
);

macro_rules! portability_directive_consolidation_test_end_semicolon {
    (
        $suite_name: ident,
        $directive: expr,
        :end_semicolon:
            $($case_name: ident = { $input: expr $(,)? }),*
        $(,)?
        :manual_semicolon:
            $($no_sc_case_name: ident = { $no_sc_input: expr $(,)? }),*
        $(,)?
    ) => {
        paste::paste! {
            casing_token_consolidation_test!(
                $suite_name,
                $(
                    $case_name = { &format!($input, $directive) },
                    [<$case_name _s>] = { &format!("{};", format!($input, $directive)) },
                )*
                $(
                    $no_sc_case_name = { &format!($no_sc_input, $directive) },
                )*
            );
        }
    }
}

macro_rules! portability_directive_consolidation_test {
    ($($case_name: ident = { $input: literal $(,)? }),* $(,)?) => {
        mod portability_directives {
            use super::*;
            $(
                portability_directive_consolidation_test_end_semicolon!(
                    $case_name,
                    $input,
                    :end_semicolon:
                        // Type declarations
                        array_type = { "type tfoo = array of boolean {}" },
                        array_type_i = { "type tfoo = array[true..false] of boolean {}" },
                        set_type = { "type tfoo = set of boolean {}" },
                        file_type = { "type tfoo = file {}" },
                        file_of_type = { "type tfoo = file of boolean {}" },
                        class_helper_type = { "type tfoo = class helper for tbar end {}" },
                        class_reference_type = { "type tfoo = class of tbar {}" },
                        class_type = { "type tfoo = class end {}" },
                        interface_type = { "type ifoo = interface end {}" },
                        object_type = { "type tfoo = object end {}" },
                        record_type = { "type tfoo = record end {}" },
                        packed_class_type = { "type tfoo = packed class end {}" },
                        packed_interface_type = { "type ifoo = packed interface end {}" },
                        packed_object_type = { "type tfoo = packed object end {}" },
                        record_helper_type = { "type tfoo = record helper for tbar end {}" },
                        pointer_type = { "type pfoo = ^tfoo {}" },
                        string_type = { "type tfoo = string(2) {}" },
                        method_type = { "type tfoo = procedure of object {}" },
                        procedure_reference_type = { "type tfoo = reference to function: integer {}" },
                        simple_procedure_type = { "type tfoo = function: integer {}" },
                        sub_range_type = { "type tfoo = ttfoo..ffbar {}" },
                        type_of_type = { "type tfoo = type of tbar {}" },
                        type_type = { "type tfoo = type tbar {}" },
                        alias_type = { "type tfoo = tbar {}" },
                        enum_type = { "type tfoo = (ttfoo, ttbar) {}" },
                        // File headers
                        library_header = { "library foo {}" },
                        package_header = { "package foo {}" },
                        unit_header = { "unit foo {}" },
                        program_header = { "program foo {}" },
                    :manual_semicolon:
                        // Const declaration
                        top_level_const = { "const foo = 1 {}" },
                        inline_const = { "begin const foo = 1 {} end" },
                        inline_const_s = { "begin const foo = 1 {}; end" },
                        subroutine_const = { "procedure foo; const bar = 1 {} begin end" },
                        subroutine_const_s = { "procedure foo; const bar = 1 {}; begin end" },
                        multiple_subroutine_const_end = { "procedure foo; const bar = 1; baz = 2 {} begin end" },
                        multiple_subroutine_const_end_s = { "procedure foo; const bar = 1; baz = 2 {}; begin end" },
                        multiple_subroutine_const_middle = { "procedure foo; const bar = 1 {}; baz = 2;" },
                        // Var declaration
                        top_level_var = { "var foo = 1 {}" },
                        inline_var = { "begin var foo = 1 {} end" },
                        inline_var_s = { "begin var foo = 1 {}; end" },
                        subroutine_var = { "procedure foo; var bar = 1 {} begin end" },
                        subroutine_var_s = { "procedure foo; var bar = 1 {}; begin end" },
                        multiple_subroutine_var_end = { "procedure foo; var bar = 1; baz = 2 {} begin end" },
                        multiple_subroutine_var_end_s = { "procedure foo; var bar = 1; baz = 2 {}; begin end" },
                        multiple_subroutine_var_middle = { "procedure foo; var bar = 1 {}; baz = 2;" },
                        // Field declaration
                        field_declaration = { "type tfoo = class ffoo: integer {} end;" },
                        field_declaration_s = { "type tfoo = class ffoo: integer {}; end;" },
                );
            )*
        }
    };
}

portability_directive_consolidation_test!(
    deprecated = { "deprecated" },
    deprecated_with_message = { "deprecated 'message'" },
    experimental = { "experimental" },
    platform = { "platform" },
    library = { "library" },
    experimental_library = { "experimental library" },
    experimental_library_repeated = { "experimental library experimental library" },
    all = { "deprecated experimental library" },
    all_with_message = { "deprecated 'message' experimental library" },
);

// Portability directives cannot occur after property declarations.
// They are treated as the next line and will not be treated as directives.
casing_token_consolidation_test!(
    property_portability_directive,
    deprecated = { "property Foo; DEPRECATED;" },
    experimental = { "property Foo; EXPERIMENTAL;" },
    platform = { "property Foo; PLATFORM;" },
);
casing_token_consolidation_test!(
    portability_directive_identifier,
    kk_const_value = { "const Foo = DEPRECATED" },
    kk_const_value_deprecated = { "const Foo = DEPRECATED deprecated" },
    kk_const_expr = { "const Foo = DEPRECATED + DEPRECATED deprecated" },
    var = { "var Foo: DEPRECATED deprecated" },
);

macro_rules! property_expression_parsing_consolidation_test(
    ($($case_name: ident = { $input: expr $(,)? }),* $(,)?) => {
        #[yare::parameterized(
            $($case_name = { &format!("type TFoo = class property foo: foo index {} nodefault end", $input) }),*
        )]
        fn expression_parsing(input: &str) {
            run_casing_token_consolidation_test(input);
        }
    };
);

property_expression_parsing_consolidation_test!(
    single_ident = { "DEFAULT" },
    addition = { "DEFAULT + DEFAULT" },
    shr = { "DEFAULT shr DEFAULT" },
    postfix = { "DEFAULT^" },
    empty_parens = { "()" },
    parens = { "(DEFAULT)" },
    parens_binary_op = { "(DEFAULT) + DEFAULT" },
    nested_parens = { "((DEFAULT))" },
    nested_parens_binary_op = { "((DEFAULT)) + DEFAULT" },
    empty_bracks = { "[]" },
    bracks = { "[DEFAULT]" },
    nested_bracks = { "[[DEFAULT]]" },
    array = { "[DEFAULT, [DEFAULT]]" },
    qualified_name = { "DEFAULT.DEFAULT" },
    function_call = { "DEFAULT(DEFAULT)" },
    function_call_multiple_args = { "DEFAULT(DEFAULT, DEFAULT)" },
);

macro_rules! param_specifier_consolidation_test {
    ($($case_name: ident = { $input: literal $(,)? }),* $(,)?) => {
        mod param_specifier {
            use super::*;
            $(
                casing_token_consolidation_test!(
                    $case_name,
                    kk_const = { &format!($input, "const") },
                    var = { &format!($input, "var") },
                    out = { &format!($input, "out") },
                );
            )*
        }
    };
}

param_specifier_consolidation_test!(
    one_arg = { "procedure Foo({} Arg);" },
    one_arg_typed = { "procedure Foo({} Arg: Integer);" },
    comma_arg = { "procedure Foo({} Arg1, Arg2: Integer);" },
    second_arg = { "procedure Foo(Arg1: Integer; {} Arg2);" },
    second_arg_typed = { "procedure Foo(Arg1: Integer; {} Arg2: String);" },
    t_one_arg = { "type TFoo = procedure Foo({} Arg);" },
    t_one_arg_typed = { "type TFoo = procedure Foo({} Arg: Integer);" },
    t_comma_arg = { "type TFoo = procedure Foo({} Arg1, Arg2: Integer);" },
    t_second_arg = { "type TFoo = procedure Foo(Arg1: Integer; {} Arg2);" },
    t_second_arg_typed = { "type TFoo = procedure Foo(Arg1: Integer; {} Arg2: String);" },
    o_one_arg = { "class operator Foo({} Arg);" },
    o_one_arg_typed = { "class operator Foo({} Arg: Integer);" },
    o_comma_arg = { "class operator Foo({} Arg1, Arg2: Integer);" },
    o_second_arg = { "class operator Foo(Arg1: Integer; {} Arg2);" },
    o_second_arg_typed = { "class operator Foo(Arg1: Integer; {} Arg2: String);" },
    v_one_arg = { "var Func: procedure({} Arg);" },
    v_one_arg_typed = { "var Func: procedure({} Arg: Integer);" },
    v_comma_arg = { "var Func: procedure({} Arg1, Arg2: Integer);" },
    v_second_arg = { "var Func: procedure(Arg1: Integer; {} Arg2);" },
    v_second_arg_typed = { "var Func: procedure(Arg1: Integer; {} Arg2: String);" },
    a_one_arg = { "Func := procedure({} Arg) begin end;" },
    a_one_arg_typed = { "Func := procedure({} Arg: Integer) begin end;" },
    a_comma_arg = { "Func := procedure({} Arg1, Arg2: Integer) begin end;" },
    a_second_arg = { "Func := procedure(Arg1: Integer; {} Arg2) begin end;" },
    a_second_arg_typed = { "Func := procedure(Arg1: Integer; {} Arg2: String) begin end;" },
);

casing_token_consolidation_test!(
    absolute,
    in_var = { "var ABSOLUTE: integer absolute bar;" },
    in_anonymous_proc_var =
        { "begin var a := procedure var ABSOLUTE: integer absolute bar begin end; end" },
    identifier = { "function ABSOLUTE(ABSOLUTE: ABSOLUTE): ABSOLUTE; begin ABSOLUTE; end;" },
);

casing_token_consolidation_test!(
    kk_abstract,
    class_state = { "type foo = class abstract end;" },
    class_state_parent = { "type foo = class abstract(parent) end;" },
    // Routine Directive tests
    identifier = { "type ABSTRACT = class ABSTRACT: ABSTRACT end;" },
);
casing_token_consolidation_test!(
    align,
    empty_record = { "type rec = record end align 1;" },
    record = { "type rec = record f: integer end align 1;" },
    record_expr = { "type rec = record f: integer end align 1 + 2 * 4;" },
    identifier = { "type ALIGN = record ALIGN: integer end;" },
);
casing_token_consolidation_test!(
    at,
    raise_at = { "raise exception at returnaddress;" },
    identifier = { "raise AT at AT;" },
);
casing_token_consolidation_test!(
    contains,
    package = { "package CONTAINS; contains CONTAINS;" },
    identifier = { "var CONTAINS: CONTAINS := CONTAINS;" },
    identifier2 = { "CONTAINS := CONTAINS;" },
);
casing_token_consolidation_test!(
    helper,
    record = { "type TFoo = record helper for HELPER end;" },
    class = { "type TFoo = class helper for HELPER end;" },
    class_with_parent = { "type TFoo = class helper(HELPER) for HELPER end;" },
    identifier = { "type HELPER = class HELPER: HELPER end;" },
);
casing_token_consolidation_test!(
    index,
    exports = { "exports foo index 1;" },
    exports_many = { "exports foo index 1, bar index 2;" },
    exports_other_directives_n = { "exports foo index 1 name 'name';" },
    exports_other_directives_r = { "exports foo index 1 resident;" },
    exports_other_directives_nr = { "exports foo index 1 name NAME resident;" },
    exports_many_other_directives =
        { "exports foo index 1 name NAME resident, bar index 2 name NAME resident" },
    // Property Directive tests
    identifier = { "exports INDEX; var INDEX: INDEX := INDEX;" },
);
casing_token_consolidation_test!(
    name,
    exports = { "exports foo name NAME;" },
    exports_many = { "exports foo name NAME, bar name NAME;" },
    exports_other_directives_i = { "exports foo index 1 name 'name';" },
    exports_other_directives_r = { "exports foo name NAME resident;" },
    exports_other_directives_ir = { "exports foo index 1 name NAME resident;" },
    exports_many_other_directives =
        { "exports foo index 1 name NAME resident, bar index 2 name NAME resident" },
    identifier = { "exports NAME; var NAME: NAME := NAME;" },
);
casing_token_consolidation_test!(
    on,
    single = { " try except on E: Exception do; end" },
    multiple = { " try except on E: Exception do; on E2: Exception do; end" },
    identifier = { " try except on ON: ON do; on ON: ON do; end" },
);
casing_token_consolidation_test!(
    operator,
    class_no_args = { "class operator foo;" },
    class_args = { "class operator foo(arg1: integer; arg2: integer);" },
    identifier = { "function OPERATOR(OPERATOR: OPERATOR): OPERATOR;" }
);
casing_token_consolidation_test!(
    class_op_in,
    // In this case, and this case only, the reserved word `in` can be used as in identifier.
    class_in = { "class operator IN(a: boolean = true in [false]): a;" },
);
casing_token_consolidation_test!(
    package,
    package = { "package foo; requires bar; contains baz;" },
    identifier = { "package PACKAGE; requires PACKAGE; contains PACKAGE;" },
);
casing_token_consolidation_test!(
    reference,
    to_fn = { "type foo = reference to function: integer;" },
    to_fn_args = { "type foo = reference to function(arg1: integer; arg2: integer): integer;" },
    to_proc = { "type foo = reference to procedure;" },
    to_proc_args = { "type foo = reference to procedure(arg1: integer; arg2: integer);" },
    identifier =
        { "type REFERENCE = function(REFERENCE: REFERENCE; REFERENCE: REFERENCE): REFERENCE;" },
);
casing_token_consolidation_test!(
    requires,
    package = { "package REQUIRES; requires REQUIRES;" },
    identifier = { "var REQUIRES: REQUIRES := REQUIRES;" },
    identifier2 = { "REQUIRES := REQUIRES;" },
);
casing_token_consolidation_test!(
    resident,
    exports = { "exports foo resident;" },
    exports_many = { "exports foo resident, bar resident;" },
    exports_other_directives_i = { "exports foo index 1 resident;" },
    exports_other_directives_n = { "exports foo name NAME resident;" },
    exports_other_directives_in = { "exports foo index 1 name NAME resident;" },
    exports_many_other_directives =
        { "exports foo index 1 name NAME resident, bar index 2 name NAME resident" },
    identifier = { "exports RESIDENT; var RESIDENT: RESIDENT := RESIDENT;" },
);
casing_token_consolidation_test!(
    sealed,
    class_state = { "type foo = class sealed end;" },
    class_state_parent = { "type foo = class sealed(SEALED) end;" },
    identifier = { "type SEALED = class SEALED: SEALED end;" },
);
casing_token_consolidation_test!(
    strict,
    identifier = { "type foo = class STRICT: STRICT; strict private STRICT: STRICT end;" },
);

/*
    In cases like these, `helper` can be both an identifier and a keyword based on the conditional
    directives.
    For the purpose of this tool, the type of the token is determined by the
    first conditional path through the code.
*/
casing_token_consolidation_test!(
    conditional_keywords,
    id_helper =
        { "type HELPER = class HELPER {$ifdef A} : integer {$else} for tobject {$endif} end;" },
    kk_helper =
        { "type HELPER = class helper {$ifdef A} for tobject {$else} : integer {$endif} end;" },
);

fn run_test(
    input: &str,
    token_selector: fn(&TokenType) -> bool,
    expected_token_types: &[TokenType],
) {
    let lexer = &DelphiLexer {};
    let parser = &DelphiLogicalLineParser {};
    eprintln!("input: {input}");
    let tokens = lexer.lex(input);
    let (_, mut tokens, _) = parser.parse(tokens);

    DistinguishGenericTypeParamsConsolidator {}.consolidate(&mut tokens);

    let actual_token_types = tokens
        .iter()
        .map(|t| t.get_token_type())
        .filter(token_selector)
        .collect_vec();

    assert_that(&&actual_token_types[..]).is_equal_to(expected_token_types);
}

const BEQ: TokenType = TokenType::Op(OK::Equal(EqKind::Comp));
const DEQ: TokenType = TokenType::Op(OK::Equal(EqKind::Decl));

#[yare::parameterized(
    type_class = { "type A = class end;", &[DEQ] },
    type_record = {"type A = record end;", &[DEQ] },
    type_proc = { "type A = procedure(B: {$if foo}Boolean{$else}Boolean{$endif} = 1 = 1; C: Boolean = 1 = 1);", &[DEQ, DEQ, BEQ, DEQ, BEQ] },
    anonymous_arg_value = { "A(procedure(B: Boolean = 1 = 1; C: Boolean = 1 = 1));", &[DEQ, BEQ, DEQ, BEQ] },
    method_resolution_clause = { "type A = class procedure B = C; end;", &[DEQ, DEQ] },
    type_enum = { "type A = (B = 1, C = 2);", &[DEQ, DEQ, DEQ] },
    const_def = { "const A = 1 = 1;", &[DEQ, BEQ] },
    inline_const_def = { "begin const A = 1 = 1; end;", &[DEQ, BEQ] },
    inline_var_def = { "begin var A := 1 = 1; end;", &[BEQ] },
)]
fn equals(input: &str, expected_token_types: &[TokenType]) {
    run_test(
        input,
        |tt| matches!(tt, TokenType::Op(OK::Equal(_))),
        expected_token_types,
    );
}

const IIN: TokenType = TokenType::Keyword(KK::In(InKind::Import));
const FIN: TokenType = TokenType::Keyword(KK::In(InKind::ForLoop));
const OIN: TokenType = TokenType::Keyword(KK::In(InKind::Op));

#[yare::parameterized(
    uses_in = { "program foo; uses a in '', b in '';", &[IIN, IIN] },
    contains_in = { "package foo; contains a in '', b in '';", &[IIN, IIN] },
    requires_in = { "package foo; requires a in '', b in '';", &[IIN, IIN] },
    for_in = { "for A in [B in [C in D]] do", &[FIN, OIN, OIN] },
    expr_in = { "A := (B in C) and (D in [E in F])", &[OIN, OIN, OIN] },
)]
fn kk_in(input: &str, expected_token_types: &[TokenType]) {
    run_test(
        input,
        |tt| matches!(tt, TokenType::Keyword(KK::In(_))),
        expected_token_types,
    );
}

const DC: TokenType = TokenType::Op(OK::Caret(CaretKind::Deref));
const TC: TokenType = TokenType::Op(OK::Caret(CaretKind::Type));

#[yare::parameterized(
    type_block = { "type A = ^B;", &[TC] },
    deref = { "A^.B^^[C^]^", &[DC, DC, DC, DC, DC] },
    routine_param = { "function A(B: ^C = D^.E^): ^F;", &[TC, DC, DC, TC] },
    prop_decl = { "property A: ^B read C^;", &[TC, DC] },
    record = { "type R = record A: ^B end;", &[TC] },
    var = { "var A: ^B;", &[TC] },
)]
fn caret(input: &str, expected_token_types: &[TokenType]) {
    run_test(
        input,
        |tt| matches!(tt, TokenType::Op(OK::Caret(_))),
        expected_token_types,
    );
}

const AVAR: TokenType = TokenType::Keyword(KK::Var(DK::AnonSection));
const SVAR: TokenType = TokenType::Keyword(KK::Var(DK::Section));
const IVAR: TokenType = TokenType::Keyword(KK::Var(DK::Inline));
const PVAR: TokenType = TokenType::Keyword(KK::Var(DK::Param));
const ACONST: TokenType = TokenType::Keyword(KK::Const(DK::AnonSection));
const SCONST: TokenType = TokenType::Keyword(KK::Const(DK::Section));
const ICONST: TokenType = TokenType::Keyword(KK::Const(DK::Inline));
const PCONST: TokenType = TokenType::Keyword(KK::Const(DK::Param));
const OCONST: TokenType = TokenType::Keyword(KK::Const(DK::Other));

#[yare::parameterized(
    anon = {
        "A := procedure(var A; const B) var C: D; const E = F; begin var G := H; const I = J; end;",
        &[PVAR, PCONST, AVAR, ACONST, IVAR, ICONST]
    },
    section = { "var A: B; const C = D;", &[SVAR, SCONST] },
    compound_inline = { "begin var A := B; const C = D; end;", &[IVAR, ICONST] },
    repeat_inline = { "repeat var A := B; const C = D; until;", &[IVAR, ICONST] },
    try_finally_inline = {
        "try var A := B; const C = D; finally var E := F; const G := H; end;",
        &[IVAR, ICONST, IVAR, ICONST]
    },
    try_bare_except_inline = { "try except var A := B; const C := D; end;", &[IVAR, ICONST] },
    try_except_on_inline = {
        "try except on A do var B := C; on D do const E := F; end;",
        &[IVAR, ICONST]
    },
    initialization_finalization_inline = {
        "initialization var A := B; const C = D; finalization var E := F; const G := H; end.",
        &[IVAR, ICONST, IVAR, ICONST]
    },
    param = { "procedure A(var B; const C);", &[PVAR, PCONST] },
    for_loop = { "for var A := B to C do", &[IVAR] },
    other = { "type A = array of const;", &[OCONST] },
)]
fn decl_kind(input: &str, expected_token_types: &[TokenType]) {
    run_test(
        input,
        |tt| matches!(tt, TokenType::Keyword(KK::Const(_) | KK::Var(_))),
        expected_token_types,
    );
}

open! Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
  open Context

  let s = T.Var.supply
  let (start, s) = variable s "start"
  let (ws, s) = variable s "ws"

  let (block, s) = variable s "block"
  let (label_declaration_part, s) = variable s "label_declaration_part"
  let (constant_definition_part, s) = variable s "constant_definition_part"
  let (type_definition_part, s) = variable s "type_definition_part"
  let (variable_declaration_part, s) = variable s "variable_declaration_part"
  let (procedure_and_function_declaration_part, s) = variable s "procedure_and_function_declaration_part"
  let (statement_part, s) = variable s "statement_part"
  let (constant_definition, s) = variable s "constant_definition"
  let (constant, s) = variable s "constant"
  let (type_definition, s) = variable s "type_definition"
  let (type_denoter, s) = variable s "type_denoter"
  let (new_type, s) = variable s "new_type"
  let (simple_type_identifier, s) = variable s "simple_type_identifier"
  let (structured_type_identifier, s) = variable s "structured_type_identifier"
  let (pointer_type_identifier, s) = variable s "pointer_type_identifier"
  let (type_identifier, s) = variable s "type_identifier"
  let (simple_type, s) = variable s "simple_type"
  let (ordinal_type, s) = variable s "ordinal_type"
  let (new_ordinal_type, s) = variable s "new_ordinal_type"
  let (ordinal_type_identifier, s) = variable s "ordinal_type_identifier"
  let (real_type_identifier, s) = variable s "real_type_identifier"
  let (enumerated_type, s) = variable s "enumerated_type"
  let (identifier_list, s) = variable s "identifier_list"
  let (subrange_type, s) = variable s "subrange_type"
  let (structured_type, s) = variable s "structured_type"
  let (new_structured_type, s) = variable s "new_structured_type"
  let (unpacked_structured_type, s) = variable s "unpacked_structured_type"
  let (array_type, s) = variable s "array_type"
  let (index_type, s) = variable s "index_type"
  let (component_type, s) = variable s "component_type"
  let (record_type, s) = variable s "record_type"
  let (field_list, s) = variable s "field_list"
  let (fixed_part, s) = variable s "fixed_part"
  let (record_section, s) = variable s "record_section"
  let (variant_part, s) = variable s "variant_part"
  let (variant_selector, s) = variable s "variant_selector"
  let (tag_field, s) = variable s "tag_field"
  let (variant, s) = variable s "variant"
  let (tag_type, s) = variable s "tag_type"
  let (case_constant_list, s) = variable s "case_constant_list"
  let (case_constant, s) = variable s "case_constant"
  let (set_type, s) = variable s "set_type"
  let (base_type, s) = variable s "base_type"
  let (file_type, s) = variable s "file_type"
  let (pointer_type, s) = variable s "pointer_type"
  let (new_pointer_type, s) = variable s "new_pointer_type"
  let (domain_type, s) = variable s "domain_type"
  let (variable_declaration, s) = variable s "variable_declaration"
  let (variable_access, s) = variable s "variable_access"
  let (entire_variable, s) = variable s "entire_variable"
  let (identified_variable, s) = variable s "identified_variable"
  let (pointer_variable, s) = variable s "pointer_variable"
  let (indexed_variable, s) = variable s "indexed_variable"
  let (array_variable, s) = variable s "array_variable"
  let (index_expression, s) = variable s "index_expression"
  let (record_variable, s) = variable s "record_variable"
  let (field_specifier, s) = variable s "field_specifier"
  let (procedure_declaration, s) = variable s "procedure_declaration"
  let (procedure_heading, s) = variable s "procedure_heading"
  let (procedure_identification, s) = variable s "procedure_identification"
  let (procedure_block, s) = variable s "procedure_block"
  let (function_declaration, s) = variable s "function_declaration"
  let (function_heading, s) = variable s "function_heading"
  let (function_identification, s) = variable s "function_identification"
  let (result_type, s) = variable s "result_type"
  let (function_block, s) = variable s "function_block"
  let (formal_parameter_list, s) = variable s "formal_parameter_list"
  let (formal_parameter_section, s) = variable s "formal_parameter_section"
  let (value_parameter_specification, s) = variable s "value_parameter_specification"
  let (variable_parameter_specification, s) = variable s "variable_parameter_specification"
  let (procedural_parameter_specification, s) = variable s "procedural_parameter_specification"
  let (functional_parameter_specification, s) = variable s "functional_parameter_specification"
  let (conformant_array_parameter_specification, s) = variable s "conformant_array_parameter_specification"
  let (value_conformant_array_specification, s) = variable s "value_conformant_array_specification"
  let (variable_conformant_array_specification, s) = variable s "variable_conformant_array_specification"
  let (conformant_array_schema, s) = variable s "conformant_array_schema"
  let (packed_conformant_array_schema, s) = variable s "packed_conformant_array_schema"
  let (unpacked_conformant_array_schema, s) = variable s "unpacked_conformant_array_schema"
  let (index_type_specification, s) = variable s "index_type_specification"
  let (expression, s) = variable s "expression"
  let (simple_expression, s) = variable s "simple_expression"
  let (term, s) = variable s "term"
  let (factor, s) = variable s "factor"
  let (unsigned_constant, s) = variable s "unsigned_constant"
  let (set_constructor, s) = variable s "set_constructor"
  let (member_designator, s) = variable s "member_designator"
  let (function_designator, s) = variable s "function_designator"
  let (actual_parameter_list, s) = variable s "actual_parameter_list"
  let (actual_parameter, s) = variable s "actual_parameter"
  let (statement, s) = variable s "statement"
  let (simple_statement, s) = variable s "simple_statement"
  let (assignment_statement, s) = variable s "assignment_statement"
  let (procedure_statement, s) = variable s "procedure_statement"
  let (goto_statement, s) = variable s "goto_statement"
  let (structured_statement, s) = variable s "structured_statement"
  let (statement_sequence, s) = variable s "statement_sequence"
  let (compound_statement, s) = variable s "compound_statement"
  let (conditional_statement, s) = variable s "conditional_statement"
  let (if_statement, s) = variable s "if_statement"
  let (else_part, s) = variable s "else_part"
  let (case_statement, s) = variable s "case_statement"
  let (case_list_element, s) = variable s "case_list_element"
  let (case_index, s) = variable s "case_index"
  let (repetitive_statement, s) = variable s "repetitive_statement"
  let (repeat_statement, s) = variable s "repeat_statement"
  let (while_statement, s) = variable s "while_statement"
  let (for_statement, s) = variable s "for_statement"
  let (control_variable, s) = variable s "control_variable"
  let (initial_value, s) = variable s "initial_value"
  let (final_value, s) = variable s "final_value"
  let (with_statement, s) = variable s "with_statement"
  let (record_variable_list, s) = variable s "record_variable_list"
  let (write_parameter, s) = variable s "write_parameter"
  let (program, s) = variable s "program"
  let (program_heading, s) = variable s "program_heading"
  let (program_parameter_list, s) = variable s "program_parameter_list"
  let (program_block, s) = variable s "program_block"
  let (constant_identifier, s) = variable s "constant_identifier"
  let (field_identifier, s) = variable s "field_identifier"
  let (component_variable, s) = variable s "component_variable"
  let (buffer_variable, s) = variable s "buffer_variable"
  let (variable_identifier, s) = variable s "variable_identifier"
  let (field_designator, s) = variable s "field_designator"
  let (field_designator_identifier, s) = variable s "field_designator_identifier"
  let (file_variable, s) = variable s "file_variable"
  let (procedure_identifier, s) = variable s "procedure_identifier"
  let (function_identifier, s) = variable s "function_identifier"
  let (bound_identifier, s) = variable s "bound_identifier"
  let (read_parameter_list, s) = variable s "read_parameter_list"
  let (readln_parameter_list, s) = variable s "readln_parameter_list"
  let (write_parameter_list, s) = variable s "write_parameter_list"
  let (writeln_parameter_list, s) = variable s "writeln_parameter_list"

  let (identifier, s) = variable s "identifier"
  let (directive, s) = variable s "directive"
  let (unsigned_number, s) = variable s "unsigned_number"
  let (sign, s) = variable s "sign"
  let (label, s) = variable s "label"
  let (character_string, s) = variable s "character_string"
  let (multiplying_operator, s) = variable s "multiplying_operator"
  let (adding_operator, s) = variable s "adding_operator"
  let (relational_operator, s) = variable s "relational_operator"
  let (comma, s) = variable s "comma"
  let (semi, s) = variable s "semi"
  let (eq, s) = variable s "eq"
  let (lparen, s) = variable s "lparen"
  let (rparen, s) = variable s "rparen"
  let (dotdot, s) = variable s "dotdot"
  let (colon, s) = variable s "colon"
  let (dot, s) = variable s "dot"
  let (lbrak, s) = variable s "lbrak"
  let (rbrak, s) = variable s "rbrak"
  let (assign, s) = variable s "assign"
  let (ptr, s) = variable s "ptr"
  let (kw_label, s) = variable s "kw_label"
  let (kw_const, s) = variable s "kw_const"
  let (kw_type, s) = variable s "kw_type"
  let (kw_var, s) = variable s "kw_var"
  let (kw_packed, s) = variable s "kw_packed"
  let (kw_array, s) = variable s "kw_array"
  let (kw_record, s) = variable s "kw_record"
  let (kw_end, s) = variable s "kw_end"
  let (kw_begin, s) = variable s "kw_begin"
  let (kw_goto, s) = variable s "kw_goto"
  let (kw_else, s) = variable s "kw_else"
  let (kw_case, s) = variable s "kw_case"
  let (kw_repeat, s) = variable s "kw_repeat"
  let (kw_while, s) = variable s "kw_while"
  let (kw_set, s) = variable s "kw_set"
  let (kw_file, s) = variable s "kw_file"
  let (kw_procedure, s) = variable s "kw_procedure"
  let (kw_function, s) = variable s "kw_function"
  let (kw_not, s) = variable s "kw_not"
  let (kw_nil, s) = variable s "kw_nil"
  let (kw_then, s) = variable s "kw_then"
  let (kw_until, s) = variable s "kw_until"
  let (kw_with, s) = variable s "kw_with"
  let (kw_program, s) = variable s "kw_program"
  let (kw_for, s) = variable s "kw_for"
  let (kw_downto, s) = variable s "kw_downto"
  let (kw_of, s) = variable s "kw_of"
  let (kw_do, s) = variable s "kw_do"
  let (kw_if, s) = variable s "kw_if"
  let (kw_to, s) = variable s "kw_to"

  let (u, s) = variable s "u"
  let (block', s) = variable s "block'"
  let (constantdefinition', s) = variable s "constantdefinition'"
  let (typedefinition', s) = variable s "typedefinition'"
  let (simpletype', s) = variable s "simpletype'"
  let (enumeratedtype', s) = variable s "enumeratedtype'"
  let (subrangetype', s) = variable s "subrangetype'"
  let (structuredtype', s) = variable s "structuredtype'"
  let (arraytype', s) = variable s "arraytype'"
  let (recordtype', s) = variable s "recordtype'"
  let (variant', s) = variable s "variant'"
  let (settype', s) = variable s "settype'"
  let (filetype', s) = variable s "filetype'"
  let (pointertype', s) = variable s "pointertype'"
  let (variabledeclaration', s) = variable s "variabledeclaration'"
  let (variableaccess', s) = variable s "variableaccess'"
  let (entirevariable', s) = variable s "entirevariable'"
  let (identifiedvariable', s) = variable s "identifiedvariable'"
  let (pointervariable', s) = variable s "pointervariable'"
  let (indexedvariable', s) = variable s "indexedvariable'"
  let (arrayvariable', s) = variable s "arrayvariable'"
  let (recordvariable', s) = variable s "recordvariable'"
  let (proceduredeclaration', s) = variable s "proceduredeclaration'"
  let (functiondeclaration', s) = variable s "functiondeclaration'"
  let (parameters', s) = variable s "parameters'"
  let (valueparameter', s) = variable s "valueparameter'"
  let (variableparameter', s) = variable s "variableparameter'"
  let (proceduralparameter', s) = variable s "proceduralparameter'"
  let (functionalparameter', s) = variable s "functionalparameter'"
  let (arrayparameter', s) = variable s "arrayparameter'"
  let (expression', s) = variable s "expression'"
  let (simpleepression', s) = variable s "simpleepression'"
  let (term', s) = variable s "term'"
  let (factor', s) = variable s "factor'"
  let (setconstructor', s) = variable s "setconstructor'"
  let (functiondesignator', s) = variable s "functiondesignator'"
  let (actualparameter', s) = variable s "actualparameter'"
  let (statement', s) = variable s "statement'"
  let (simplestatement', s) = variable s "simplestatement'"
  let (assignmentstatement', s) = variable s "assignmentstatement'"
  let (procedurestatement', s) = variable s "procedurestatement'"
  let (gotostatement', s) = variable s "gotostatement'"
  let (structuredstatement', s) = variable s "structuredstatement'"
  let (compoundstatement', s) = variable s "compoundstatement'"
  let (ifstatement', s) = variable s "ifstatement'"
  let (elsepart', s) = variable s "elsepart'"
  let (casestatement', s) = variable s "casestatement'"
  let (caseelement', s) = variable s "caseelement'"
  let (repeatstatement', s) = variable s "repeatstatement'"
  let (whilestatement', s) = variable s "whilestatement'"
  let (forstatement', s) = variable s "forstatement'"
  let (withstatement', s) = variable s "withstatement'"
  let (program', s) = variable s "program'"
  let (componentvariable', s) = variable s "componentvariable'"
  let (fielddesignator', s) = variable s "fielddesignator'"
  let (buffervariable', s) = variable s "buffervariable'"
  let (filevariable', s) = variable s "filevariable'"

  let syntactic = [
    start;
    block;
    label_declaration_part;
    constant_definition_part;
    type_definition_part;
    variable_declaration_part;
    procedure_and_function_declaration_part;
    statement_part;
    constant_definition;
    constant;
    type_definition;
    type_denoter;
    new_type;
    simple_type_identifier;
    structured_type_identifier;
    pointer_type_identifier;
    type_identifier;
    simple_type;
    ordinal_type;
    new_ordinal_type;
    ordinal_type_identifier;
    real_type_identifier;
    enumerated_type;
    identifier_list;
    subrange_type;
    structured_type;
    new_structured_type;
    unpacked_structured_type;
    array_type;
    index_type;
    component_type;
    record_type;
    field_list;
    fixed_part;
    record_section;
    variant_part;
    variant_selector;
    tag_field;
    variant;
    tag_type;
    case_constant_list;
    case_constant;
    set_type;
    base_type;
    file_type;
    pointer_type;
    new_pointer_type;
    domain_type;
    variable_declaration;
    variable_access;
    entire_variable;
    identified_variable;
    pointer_variable;
    indexed_variable;
    array_variable;
    index_expression;
    record_variable;
    field_specifier;
    procedure_declaration;
    procedure_heading;
    procedure_identification;
    procedure_block;
    function_declaration;
    function_heading;
    function_identification;
    result_type;
    function_block;
    formal_parameter_list;
    formal_parameter_section;
    value_parameter_specification;
    variable_parameter_specification;
    procedural_parameter_specification;
    functional_parameter_specification;
    conformant_array_parameter_specification;
    value_conformant_array_specification;
    variable_conformant_array_specification;
    conformant_array_schema;
    packed_conformant_array_schema;
    unpacked_conformant_array_schema;
    index_type_specification;
    expression;
    simple_expression;
    term;
    factor;
    unsigned_constant;
    set_constructor;
    member_designator;
    function_designator;
    actual_parameter_list;
    actual_parameter;
    statement;
    simple_statement;
    assignment_statement;
    procedure_statement;
    goto_statement;
    structured_statement;
    statement_sequence;
    compound_statement;
    conditional_statement;
    if_statement;
    else_part;
    case_statement;
    case_list_element;
    case_index;
    repetitive_statement;
    repeat_statement;
    while_statement;
    for_statement;
    control_variable;
    initial_value;
    final_value;
    with_statement;
    record_variable_list;
    write_parameter;
    program;
    program_heading;
    program_parameter_list;
    program_block;
    constant_identifier;
    field_identifier;
    component_variable;
    buffer_variable;
    variable_identifier;
    field_designator;
    field_designator_identifier;
    file_variable;
    procedure_identifier;
    function_identifier;
    bound_identifier;
    read_parameter_list;
    readln_parameter_list;
    write_parameter_list;
    writeln_parameter_list;
  ]

  let lexical = [
    ws;
    identifier;
    directive;
    unsigned_number;
    sign;
    label;
    character_string;
    multiplying_operator;
    adding_operator;
    relational_operator;
    comma;
    semi;
    eq;
    lparen;
    rparen;
    dotdot;
    colon;
    dot;
    lbrak;
    rbrak;
    assign;
    ptr;
    kw_label;
    kw_const;
    kw_type;
    kw_var;
    kw_packed;
    kw_array;
    kw_record;
    kw_end;
    kw_begin;
    kw_goto;
    kw_else;
    kw_case;
    kw_repeat;
    kw_while;
    kw_set;
    kw_file;
    kw_procedure;
    kw_function;
    kw_not;
    kw_nil;
    kw_then;
    kw_until;
    kw_with;
    kw_program;
    kw_for;
    kw_downto;
    kw_of;
    kw_do;
    kw_if;
    kw_to;
  ]

  let longest_match = [
    ws;
    identifier;
    directive;
  ]

  let start = start

  let parser =
    with_ws (T.Vars.of_list lexical) (var ws) @@ unextend s u Production.[
      make (u, start) R.(var program * plus eof);

      make (block', block) R.(opt (var label_declaration_part) * opt (var constant_definition_part) * opt (var type_definition_part) * opt (var variable_declaration_part) * star (var procedure_and_function_declaration_part) * var statement_part);

      make (u, label_declaration_part) R.(var kw_label * var label * star (var comma * var label) * var semi);

      make (u, constant_definition_part) R.(var kw_const * var constant_definition * var semi * star (var constant_definition * var semi));

      make (u, type_definition_part) R.(var kw_type * var type_definition * var semi * star (var type_definition * var semi));

      make (u, variable_declaration_part) R.(var kw_var * var variable_declaration * var semi * star (var variable_declaration * var semi));

      make (u, procedure_and_function_declaration_part) R.(star ((var procedure_declaration + var function_declaration) * var semi));

      make (u, statement_part) (var compound_statement);

      make (constantdefinition', constant_definition) R.(var identifier * var eq * var constant);

      make (u, constant) R.(opt (var sign) * (var unsigned_number + var constant_identifier) + var character_string);

      make (u, constant_identifier) (var identifier);

      make (typedefinition', type_definition) R.(var identifier * var eq * var type_denoter);

      make (u, type_denoter) R.(var type_identifier + var new_type);

      make (u, new_type) R.(var new_ordinal_type + var new_structured_type + var new_pointer_type);

      make (u, simple_type_identifier) (var type_identifier);

      make (u, structured_type_identifier) (var type_identifier);

      make (u, pointer_type_identifier) (var type_identifier);

      make (u, type_identifier) (var identifier);

      make (simpletype', simple_type) R.(var ordinal_type + var real_type_identifier);

      make (u, ordinal_type) R.(var new_ordinal_type + var ordinal_type_identifier);

      make (u, new_ordinal_type) R.(var enumerated_type + var subrange_type);

      make (u, ordinal_type_identifier) (var type_identifier);

      make (u, real_type_identifier) (var type_identifier);

      make (enumeratedtype', enumerated_type) R.(var lparen * var identifier_list * var rparen);

      make (u, identifier_list) R.(var identifier * star (var comma * var identifier));

      make (subrangetype', subrange_type) R.(var constant * var dotdot * var constant);

      make (structuredtype', structured_type) R.(var new_structured_type + var structured_type_identifier);

      make (u, new_structured_type) R.(opt (var kw_packed) * var unpacked_structured_type);

      make (u, unpacked_structured_type) R.(var array_type + var record_type + var set_type + var file_type);

      make (arraytype', array_type) R.(var kw_array * var lbrak * var index_type * star (var comma * var index_type) * var rbrak * var kw_of * var component_type);

      make (u, index_type) (var ordinal_type);

      make (u, component_type) (var type_denoter);

      make (recordtype', record_type) R.(var kw_record * opt (var field_list) * var kw_end);

      make (u, field_list) R.((var fixed_part * opt (var semi * var variant_part) + var variant_part) * opt (var semi));

      make (u, fixed_part) R.(var record_section * star (var semi * var record_section));

      make (u, record_section) R.(var identifier_list * var colon * var type_denoter);

      make (u, field_identifier) (var identifier);

      make (u, variant_part) R.(var kw_case * var variant_selector * var kw_of * var variant * star (var semi * var variant));

      make (u, variant_selector) R.(opt (var tag_field * var colon) * var tag_type);

      make (u, tag_field) (var identifier);

      make (variant', variant) R.(var case_constant_list * var colon * var lparen * var field_list * var rparen);

      make (u, tag_type) (var ordinal_type_identifier);

      make (u, case_constant_list) R.(var case_constant * star (var comma * var case_constant));

      make (u, case_constant) (var constant);

      make (settype', set_type) R.(var kw_set * var kw_of * var base_type);

      make (u, base_type) (var ordinal_type);

      make (filetype', file_type) R.(var kw_file * var kw_of * var component_type);

      make (pointertype', pointer_type) R.(var new_pointer_type + var pointer_type_identifier);

      make (u, new_pointer_type) R.(var ptr * var domain_type);

      make (u, domain_type) (var type_identifier);

      make (variabledeclaration', variable_declaration) R.(var identifier_list * var colon * var type_denoter);

      make (variableaccess', variable_access) R.(var entire_variable + var component_variable + var identified_variable + var buffer_variable);

      make (entirevariable', entire_variable) (var variable_identifier);

      make (identifiedvariable', identified_variable) R.(var pointer_variable * var ptr);

      make (pointervariable', pointer_variable) (var variable_access);

      make (u, variable_identifier) (var identifier);

      make (componentvariable', component_variable) R.(var indexed_variable + var field_designator);

      make (indexedvariable', indexed_variable) R.(var array_variable * var lbrak * var index_expression * star (var comma * var index_expression) * var rbrak);

      make (arrayvariable', array_variable) (var variable_access);

      make (u, index_expression) (var expression);

      make (fielddesignator', field_designator) R.(var record_variable * var dot * var field_specifier + var field_designator_identifier);

      make (recordvariable', record_variable) (var variable_access);

      make (u, field_specifier) (var field_identifier);

      make (buffervariable', buffer_variable) R.(var file_variable * var ptr);

      make (filevariable', file_variable) (var variable_access);


      make (proceduredeclaration', procedure_declaration) R.(var procedure_heading * var semi * var directive + var procedure_identification * var semi * var procedure_block + var procedure_heading * var semi * var procedure_block);

      make (u, procedure_heading) R.(var kw_procedure * var identifier * var formal_parameter_list);

      make (u, procedure_identification) R.(var kw_procedure * var procedure_identifier);

      make (u, procedure_identifier) (var identifier);

      make (u, procedure_block) (var block);


      make (functiondeclaration', function_declaration) R.(var function_heading * var semi * var directive + var function_identification * var semi * var function_block + var function_heading * var semi * var function_block);


      make (u, function_heading) R.(var kw_function * var identifier * opt (var formal_parameter_list) * var colon * var result_type);

      make (u, function_identification) R.(var kw_function * var function_identifier);

      make (u, function_identifier) (var identifier);

      make (u, result_type) R.(var simple_type_identifier + var pointer_type_identifier);

      make (u, function_block) (var block);

      make (parameters', formal_parameter_list) R.(var lparen * var formal_parameter_section * star (var semi * var formal_parameter_section) * var rparen);

      make (u, formal_parameter_section) R.(var value_parameter_specification + var variable_parameter_specification + var procedural_parameter_specification + var functional_parameter_specification + var conformant_array_parameter_specification);

      make (valueparameter', value_parameter_specification) R.(var identifier_list * var colon * var type_identifier);

      make (variableparameter', variable_parameter_specification) R.(var kw_var * var identifier_list * var colon * var type_identifier);

      make (proceduralparameter', procedural_parameter_specification) (var procedure_heading);

      make (functionalparameter', functional_parameter_specification) (var function_heading);

      make (arrayparameter', conformant_array_parameter_specification) R.(var value_conformant_array_specification + var variable_conformant_array_specification);

      make (u, value_conformant_array_specification) R.(var identifier_list * var colon * var conformant_array_schema);

      make (u, variable_conformant_array_specification) R.(var kw_var * var identifier_list * var colon * var conformant_array_schema);

      make (u, conformant_array_schema) R.(var packed_conformant_array_schema + var unpacked_conformant_array_schema);

      make (u, packed_conformant_array_schema) R.(var kw_packed * var kw_array * var lbrak * var index_type_specification * var rbrak * var kw_of * var type_identifier);

      make (u, unpacked_conformant_array_schema) R.(var kw_array * var lbrak * var index_type_specification * star (var semi * var index_type_specification) * var rbrak * var kw_of * (var type_identifier + var conformant_array_schema));

      make (u, index_type_specification) R.(var identifier * var dotdot * var identifier * var colon * var ordinal_type_identifier);

      make (expression', expression) R.(var simple_expression * star (var relational_operator * var simple_expression));

      make (simpleepression', simple_expression) R.(opt (var sign) * var term * star (var adding_operator * var term));

      make (term', term) R.(var factor * star (var multiplying_operator * var factor));

      make (factor', factor) R.(var bound_identifier + var variable_access + var unsigned_constant + var function_designator + var set_constructor + var lparen * var expression * var rparen + var kw_not * var factor);

      make (u, bound_identifier) (var identifier);

      make (u, unsigned_constant) R.(var unsigned_number + var character_string + var constant_identifier + var kw_nil);

      make (setconstructor', set_constructor) R.(var lbrak * opt (var member_designator * star (var comma * var member_designator)) * var rbrak);

      make (u, member_designator) R.(var expression * opt (var dotdot * var expression));

      make (functiondesignator', function_designator) R.(var function_identifier * opt (var actual_parameter_list));

      make (u, actual_parameter_list) R.(var lparen * var actual_parameter * star (var comma * var actual_parameter) * var rparen);

      make (actualparameter', actual_parameter) R.(var expression + var variable_access + var procedure_identifier + var function_identifier);

      make (statement', statement) R.(opt (var label * var colon) * (var simple_statement + var structured_statement));

      make (simplestatement', simple_statement) R.(var assignment_statement + var procedure_statement + var goto_statement);

      make (assignmentstatement', assignment_statement) R.((var variable_access + var function_identifier) * var assign * var expression);

      make (procedurestatement', procedure_statement) R.(var procedure_identifier * opt ((var actual_parameter_list + var read_parameter_list + var readln_parameter_list + var write_parameter_list + var writeln_parameter_list)));

      make (gotostatement', goto_statement) R.(var kw_goto * var label);

      make (structuredstatement', structured_statement) R.(var compound_statement + var conditional_statement + var repetitive_statement + var with_statement);

      make (u, statement_sequence) R.(var statement * star (var semi * opt (var statement)));

      make (compoundstatement', compound_statement) R.(var kw_begin * opt (var statement_sequence) * var kw_end);

      make (u, conditional_statement) R.(var if_statement + var case_statement);

      make (ifstatement', if_statement) R.(var kw_if * var expression * var kw_then * var statement * opt (var else_part));

      make (elsepart', else_part) R.(var kw_else * var statement);

      make (casestatement', case_statement) R.(var kw_case * var case_index * var kw_of * var case_list_element * star (var semi * var case_list_element) * opt (var semi) * var kw_end);

      make (caseelement', case_list_element) R.(var case_constant_list * var colon * var statement);

      make (u, case_index) (var expression);

      make (u, repetitive_statement) R.(var repeat_statement + var while_statement + var for_statement);

      make (repeatstatement', repeat_statement) R.(var kw_repeat * opt (var statement_sequence) * var kw_until * var expression);

      make (whilestatement', while_statement) R.(var kw_while * var expression * var kw_do * var statement);

      make (forstatement', for_statement) R.(var kw_for * var control_variable * var assign * var initial_value * (var kw_to + var kw_downto) * var final_value * var kw_do * var statement);

      make (u, control_variable) (var entire_variable);

      make (u, initial_value) (var expression);

      make (u, final_value) (var expression);

      make (withstatement', with_statement) R.(var kw_with * var record_variable_list * var kw_do * var statement);

      make (u, record_variable_list) R.(var record_variable * star (var comma * var record_variable));

      make (u, field_designator_identifier) (var identifier);

      make (u, read_parameter_list) R.(var lparen * opt (var file_variable * var comma) * var variable_access * star (var comma * var variable_access) * var rparen);

      make (u, readln_parameter_list) R.(var lparen * (var file_variable + var variable_access) * star (var comma * var variable_access) * var rparen);

      make (u, write_parameter_list) R.(var lparen * opt (var file_variable * var comma) * var write_parameter * star (var comma * var write_parameter) * var rparen);

      make (u, write_parameter) R.(var expression * opt (var colon * var expression * opt (var colon * var expression)));

      make (u, writeln_parameter_list) R.(var lparen * (var file_variable + var write_parameter) * star (var comma * var write_parameter) * var rparen);

      make (program', program) R.(var program_heading * var semi * var program_block * var dot);

      make (u, program_heading) R.(var kw_program * var identifier * opt (var lparen * var program_parameter_list * var rparen));

      make (u, program_parameter_list) (var identifier_list);

      make (u, program_block) (var block);
    ]

  let scanner =
    let digit = range "0" "9" in
    let letter = R.(range "A" "Z" + range "a" "z" + codes "_") in
    let digit_sequence = R.(plus digit) in
    let sign_ = codes "+-" in 
    let scale_factor = R.(opt sign_ * digit_sequence) in
    let fractional_part = digit_sequence in
    let unsigned_integer = digit_sequence in
    let unsigned_real = R.(digit_sequence * codes "." * fractional_part * opt (codes "Ee" * scale_factor)) in
    let apostrophe_image = R.(codes "'" * codes "'") in
    let string_character = not_codes "'" in
    let string_element = R.(apostrophe_image + string_character) in
    Production.[
      make (u, identifier) R.(letter * star (letter + digit));
      make (u, directive) R.(letter * star (letter + digit));
      make (u, unsigned_number) R.(unsigned_integer + unsigned_real);
      make (u, sign) sign_;
      make (u, label) digit_sequence;
      make (u, character_string) R.(codes "'" * star string_element * codes "'");
      make (u, multiplying_operator) R.(codes "*/" + text "div" + text "mod" + text "and");
      make (u, adding_operator) R.(codes "+-" + text "or");
      make (u, relational_operator) R.(codes "=<>" + text "<>" + text ">=" + text "in");

      make (u, comma) (text ",");
      make (u, semi) (text ";");
      make (u, eq) (text "=");
      make (u, lparen) (text "(");
      make (u, rparen) (text ")");
      make (u, dotdot) (text "..");
      make (u, colon) (text ":");
      make (u, dot) (text ".");
      make (u, lbrak) (text "[");
      make (u, rbrak) (text "]");
      make (u, assign) (text ":=");
      make (u, ptr) (text "^");
      make (u, kw_label) (text "label");
      make (u, kw_const) (text "const");
      make (u, kw_type) (text "type");
      make (u, kw_var) (text "var");
      make (u, kw_packed) (text "packed");
      make (u, kw_array) (text "array");
      make (u, kw_record) (text "record");
      make (u, kw_end) (text "end");
      make (u, kw_begin) (text "begin");
      make (u, kw_goto) (text "goto");
      make (u, kw_else) (text "else");
      make (u, kw_case) (text "case");
      make (u, kw_repeat) (text "repeat");
      make (u, kw_while) (text "while");
      make (u, kw_set) (text "set");
      make (u, kw_file) (text "file");
      make (u, kw_procedure) (text "procedure");
      make (u, kw_function) (text "function");
      make (u, kw_not) (text "not");
      make (u, kw_nil) (text "nil");
      make (u, kw_then) (text "then");
      make (u, kw_until) (text "until");
      make (u, kw_with) (text "with");
      make (u, kw_program) (text "program");
      make (u, kw_for) (text "for");
      make (u, kw_downto) (text "downto");
      make (u, kw_of) (text "of");
      make (u, kw_do) (text "do");
      make (u, kw_if) (text "if");
      make (u, kw_to) (text "to");

      make (u, ws) R.(star (codes " \n\r\t"));
    ]

end)

let _ =
  (*let t = X.tables () in
  let d = X.driver t in
  X.Run.file (fun c -> d#read c) "seed.pas";
  Fmt.pr "@[%a@]" Trace.pp d#trace;*)
  (*Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))*)

  let t = X.tables () in
  let fs = Sys.readdir "linear" in
  Array.sort (fun x y ->
      let c = Int.compare (String.length x) (String.length y) in
      if c <> 0 then c else
      String.compare x y)
    fs;
  Array.iter (fun file ->
      let d = X.driver t in
      let t = Sys.time() in
      X.Run.file (fun c -> d#read c) ("linear/" ^ file);
      Fmt.pr "%s %b %f@." file d#accept (Sys.time() -. t))
    fs

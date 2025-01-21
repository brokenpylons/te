open Te_bot
open Te_core
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
  open Context

  let Vector.[
      start;
      ws;

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
      unsigned_simple_expression;
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

      u;
      block';
      constantdefinition';
      typedefinition';
      simpletype';
      enumeratedtype';
      subrangetype';
      structuredtype';
      arraytype';
      recordtype';
      variant';
      settype';
      filetype';
      pointertype';
      variabledeclaration';
      variableaccess';
      entirevariable';
      identifiedvariable';
      pointervariable';
      indexedvariable';
      arrayvariable';
      recordvariable';
      proceduredeclaration';
      functiondeclaration';
      parameters';
      valueparameter';
      variableparameter';
      proceduralparameter';
      functionalparameter';
      arrayparameter';
      expression';
      simpleepression';
      unsignedsimpleexpression';
      term';
      factor';
      setconstructor';
      functiondesignator';
      actualparameter';
      statement';
      simplestatement';
      assignmentstatement';
      procedurestatement';
      gotostatement';
      structuredstatement';
      compoundstatement';
      ifstatement';
      elsepart';
      casestatement';
      caseelement';
      repeatstatement';
      whilestatement';
      forstatement';
      withstatement';
      program';
    ] = variables Vector.[
      "start";
      "ws";

      "block";
      "label_declaration_part";
      "constant_definition_part";
      "type_definition_part";
      "variable_declaration_part";
      "procedure_and_function_declaration_part";
      "statement_part";
      "constant_definition";
      "constant";
      "type_definition";
      "type_denoter";
      "new_type";
      "simple_type_identifier";
      "structured_type_identifier";
      "pointer_type_identifier";
      "type_identifier";
      "simple_type";
      "ordinal_type";
      "new_ordinal_type";
      "ordinal_type_identifier";
      "real_type_identifier";
      "enumerated_type";
      "identifier_list";
      "subrange_type";
      "structured_type";
      "new_structured_type";
      "unpacked_structured_type";
      "array_type";
      "index_type";
      "component_type";
      "record_type";
      "field_list";
      "fixed_part";
      "record_section";
      "variant_part";
      "variant_selector";
      "tag_field";
      "variant";
      "tag_type";
      "case_constant_list";
      "case_constant";
      "set_type";
      "base_type";
      "file_type";
      "pointer_type";
      "new_pointer_type";
      "domain_type";
      "variable_declaration";
      "variable_access";
      "entire_variable";
      "identified_variable";
      "pointer_variable";
      "indexed_variable";
      "array_variable";
      "index_expression";
      "record_variable";
      "field_specifier";
      "procedure_declaration";
      "procedure_heading";
      "procedure_identification";
      "procedure_block";
      "function_declaration";
      "function_heading";
      "function_identification";
      "result_type";
      "function_block";
      "formal_parameter_list";
      "formal_parameter_section";
      "value_parameter_specification";
      "variable_parameter_specification";
      "procedural_parameter_specification";
      "functional_parameter_specification";
      "conformant_array_parameter_specification";
      "value_conformant_array_specification";
      "variable_conformant_array_specification";
      "conformant_array_schema";
      "packed_conformant_array_schema";
      "unpacked_conformant_array_schema";
      "index_type_specification";
      "expression";
      "simple_expression";
      "unsigned_simple_expression";
      "term";
      "factor";
      "unsigned_constant";
      "set_constructor";
      "member_designator";
      "function_designator";
      "actual_parameter_list";
      "actual_parameter";
      "statement";
      "simple_statement";
      "assignment_statement";
      "procedure_statement";
      "goto_statement";
      "structured_statement";
      "statement_sequence";
      "compound_statement";
      "conditional_statement";
      "if_statement";
      "else_part";
      "case_statement";
      "case_list_element";
      "case_index";
      "repetitive_statement";
      "repeat_statement";
      "while_statement";
      "for_statement";
      "control_variable";
      "initial_value";
      "final_value";
      "with_statement";
      "record_variable_list";
      "write_parameter";
      "program";
      "program_heading";
      "program_parameter_list";
      "program_block";

      "identifier";
      "directive";
      "unsigned_number";
      "sign";
      "label";
      "character_string";
      "multiplying_operator";
      "adding_operator";
      "relational_operator";
      "comma";
      "semi";
      "eq";
      "lparen";
      "rparen";
      "dotdot";
      "colon";
      "dot";
      "kw_label";
      "kw_const";
      "kw_type";
      "kw_var";
      "kw_packed";
      "kw_array";
      "kw_record";
      "kw_end";
      "kw_begin";
      "kw_goto";
      "kw_else";
      "kw_case";
      "kw_repeat";
      "kw_while";
      "kw_set";
      "kw_file";
      "kw_procedure";
      "kw_function";
      "kw_not";
      "kw_nil";
      "kw_then";
      "kw_until";
      "kw_with";
      "kw_program";
      "kw_for";
      "kw_downto";

      "_";
      "block'";
      "constantdefinition'";
      "typedefinition'";
      "simpletype'";
      "enumeratedtype'";
      "subrangetype'";
      "structuredtype'";
      "arraytype'";
      "recordtype'";
      "variant'";
      "settype'";
      "filetype'";
      "pointertype'";
      "variabledeclaration'";
      "variableaccess'";
      "entirevariable'";
      "identifiedvariable'";
      "pointervariable'";
      "indexedvariable'";
      "arrayvariable'";
      "recordvariable'";
      "proceduredeclaration'";
      "functiondeclaration'";
      "parameters'";
      "valueparameter'";
      "variableparameter'";
      "proceduralparameter'";
      "functionalparameter'";
      "arrayparameter'";
      "expression'";
      "simpleepression'";
      "unsignedsimpleexpression'";
      "term'";
      "factor'";
      "setconstructor'";
      "functiondesignator'";
      "actualparameter'";
      "statement'";
      "simplestatement'";
      "assignmentstatement'";
      "procedurestatement'";
      "gotostatement'";
      "structuredstatement'";
      "compoundstatement'";
      "ifstatement'";
      "elsepart'";
      "casestatement'";
      "caseelement'";
      "repeatstatement'";
      "whilestatement'";
      "forstatement'";
      "withstatement'";
      "program'";
    ]

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
    unsigned_simple_expression;
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
  ]

  let longest_match = [
    ws;
    identifier;
    directive;
  ]

  let start = start

  let parser =
    with_ws (T.Vars.of_list lexical) (var ws) Production.[
      make (u, start) R.(var program * plus eof);

      make (block', block) R.(opt (var label_declaration_part) * opt (var constant_definition_part) * opt (var type_definition_part) * opt (var variable_declaration_part) * star (var procedure_and_function_declaration_part) * var statement_part);

      make (u, label_declaration_part) R.(var kw_label * var label * star (var comma * var label) * var semi);

      make (u, constant_definition_part) R.((var kw_const * var constant_definition * var semi) * star (var constant_definition * var semi));

      make (u, type_definition_part) R.((var kw_type * var type_definition * var semi) * star (var type_definition * var semi));

      make (u, variable_declaration_part) R.((var kw_var * var variable_declaration * var semi) * star (var variable_declaration * var semi));

      make (u, procedure_and_function_declaration_part) R.((var procedure_declaration + var function_declaration) * var semi);

      make (u, statement_part) (var compound_statement);

      make (constantdefinition', constant_definition) R.(var identifier * var eq * var constant);

      make (u, constant) R.(var sign * (var unsigned_number + var identifier) + var unsigned_number + var identifier + var character_string);

      make (typedefinition', type_definition) R.(var identifier * var eq * var type_denoter);

      make (u, type_denoter) R.(var identifier + var new_type);

      make (u, new_type) R.(var new_ordinal_type + var new_structured_type + var new_pointer_type);

      make (u, simple_type_identifier) (var type_identifier);

      make (u, structured_type_identifier) (var type_identifier);

      make (u, pointer_type_identifier) (var type_identifier);

      make (u, type_identifier) (var identifier);

      make (simpletype', simple_type) R.(var ordinal_type + var identifier);

      make (u, ordinal_type) R.(var new_ordinal_type + var identifier);

      make (u, new_ordinal_type) R.(var enumerated_type + var subrange_type);

      make (u, ordinal_type_identifier) (var type_identifier);

      make (u, real_type_identifier) (var type_identifier);

      make (enumeratedtype', enumerated_type) R.(var lparen * var identifier_list * var rparen);

      make (u, identifier_list) R.(var identifier * star (var comma * var identifier));

      make (subrangetype', subrange_type) R.(var constant * var dotdot * var constant);

      make (structuredtype', structured_type) R.(var new_structured_type + var identifier);

      make (u, new_structured_type) R.(opt (var kw_packed) * var unpacked_structured_type);

      make (u, unpacked_structured_type) R.(var array_type + var record_type + var set_type + var file_type);

      make (arraytype', array_type) R.(var kw_array * var dot * var index_type * star (var comma * var index_type) * var dot * var dotdot * var component_type);

      make (u, index_type) (var ordinal_type);

      make (u, component_type) (var type_denoter);

      make (recordtype', record_type) R.(var kw_record * opt (var field_list) * var kw_end);

      make (u, field_list) R.(var fixed_part * opt (var semi * var variant_part) + var variant_part);

      make (u, fixed_part) R.(var fixed_part * var semi * var record_section + var record_section);

      make (u, record_section) R.(var identifier_list * var colon * var type_denoter);

      make (u, variant_part) R.(var kw_case * var variant_selector * var dotdot * var variant * star (var semi * var variant));

      make (u, variant_selector) R.(var tag_field * var colon * var tag_type + var tag_type);

      make (u, tag_field) (var identifier);

      make (variant', variant) R.(var case_constant_list * var colon * var lparen * var field_list * var rparen);

      make (u, tag_type) (var identifier);

      make (u, case_constant_list) R.(var case_constant * star (var comma * var case_constant));

      make (u, case_constant) (var constant);

      make (settype', set_type) R.(var kw_set * var dotdot * var base_type);

      make (u, base_type) (var ordinal_type);

      make (filetype', file_type) R.(var kw_file * var dotdot * var component_type);

      make (pointertype', pointer_type) R.(var new_pointer_type + var identifier);

      make (u, new_pointer_type) R.(var dot * var domain_type);

      make (u, domain_type) (var identifier);

      make (variabledeclaration', variable_declaration) R.(var identifier_list * var colon * var type_denoter);

      make (variableaccess', variable_access) R.(var identified_variable + var indexed_variable + var record_variable * var dot * var field_specifier + var identifier);

      make (entirevariable', entire_variable) (var identifier);

      make (identifiedvariable', identified_variable) R.(var pointer_variable * var dot);

      make (pointervariable', pointer_variable) (var variable_access);

      make (indexedvariable', indexed_variable) R.(var array_variable * var dot * var index_expression * star (var comma * var index_expression) * var dot);

      make (arrayvariable', array_variable) (var variable_access);

      make (u, index_expression) (var expression);

      make (recordvariable', record_variable) (var variable_access);

      make (u, field_specifier) (var identifier);

      make (proceduredeclaration', procedure_declaration) R.(var procedure_heading * var semi * var directive + var procedure_identification * var semi * var procedure_block + var procedure_heading * var semi * var procedure_block);

      make (u, procedure_heading) R.(var kw_procedure * var identifier * var formal_parameter_list);

      make (u, procedure_identification) R.(var kw_procedure * var identifier);

      make (u, procedure_block) (var block);

      make (functiondeclaration', function_declaration) R.(var function_heading * var semi * var directive + var function_identification * var semi * var function_block + var function_heading * var semi * var function_block);


      make (u, function_heading) R.(var kw_function * var identifier * opt (var formal_parameter_list) * var colon * var result_type);

      make (u, function_identification) R.(var kw_function * var identifier);

      make (u, result_type) (var identifier);

      make (u, function_block) (var block);

      make (parameters', formal_parameter_list) R.(var lparen * var formal_parameter_section * star (var semi * var formal_parameter_section) * var rparen);

      make (u, formal_parameter_section) R.(var value_parameter_specification + var variable_parameter_specification + var procedural_parameter_specification + var functional_parameter_specification + var conformant_array_parameter_specification);

      make (valueparameter', value_parameter_specification) R.(var identifier_list * var colon * var identifier);

      make (variableparameter', variable_parameter_specification) R.(var kw_var * var identifier_list * var colon * var identifier);

      make (proceduralparameter', procedural_parameter_specification) (var procedure_heading);

      make (functionalparameter', functional_parameter_specification) (var function_heading);

      make (arrayparameter', conformant_array_parameter_specification) R.(var value_conformant_array_specification + var variable_conformant_array_specification);

      make (u, value_conformant_array_specification) R.(var identifier_list * var colon * var conformant_array_schema);

      make (u, variable_conformant_array_specification) R.(var kw_var * var identifier_list * var colon * var conformant_array_schema);

      make (u, conformant_array_schema) R.(var packed_conformant_array_schema + var unpacked_conformant_array_schema);

      make (u, packed_conformant_array_schema) R.(var kw_packed * var kw_array * var dot * var index_type_specification * var dot * var dotdot * var identifier);

      make (u, unpacked_conformant_array_schema) R.(var kw_array * var dot * var index_type_specification * star (var semi * var index_type_specification) * var dot * var dotdot * (var identifier + var conformant_array_schema));

      make (u, index_type_specification) R.(var identifier * var dotdot * var identifier * var colon * var ordinal_type_identifier);

      make (expression', expression) R.(var simple_expression * star (var relational_operator * var simple_expression));

      make (simpleepression', simple_expression) R.(var sign * var unsigned_simple_expression + var unsigned_simple_expression);

      make (unsignedsimpleexpression', unsigned_simple_expression) R.(var term * star (var adding_operator * var term));

      make (term', term) R.(var factor * star (var multiplying_operator * var factor));

      make (factor', factor) R.(var variable_access + var unsigned_constant + var function_designator + var set_constructor + var lparen * var expression * var rparen + var kw_not * var factor);

      make (u, unsigned_constant) R.(var unsigned_number + var character_string + var kw_nil);

      make (setconstructor', set_constructor) R.(var dot * opt (var member_designator * star (var comma * var member_designator)) * var dot);

      make (u, member_designator) R.(var expression * opt (var dotdot * var expression));

      make (functiondesignator', function_designator) R.(var identifier * var actual_parameter_list);

      make (u, actual_parameter_list) R.(var lparen * var actual_parameter * star (var comma * star (var actual_parameter + var write_parameter)) * var rparen);

      make (actualparameter', actual_parameter) (var expression);

      make (statement', statement) R.(opt (var label * var colon) * (var simple_statement + var structured_statement));

      make (simplestatement', simple_statement) R.(var assignment_statement + var procedure_statement + var goto_statement);

      make (assignmentstatement', assignment_statement) R.(var variable_access * var dotdot * var expression);

      make (procedurestatement', procedure_statement) R.(var identifier * opt (var actual_parameter_list));

      make (gotostatement', goto_statement) R.(var kw_goto * var label);

      make (structuredstatement', structured_statement) R.(var compound_statement + var conditional_statement + var repetitive_statement + var with_statement);

      make (u, statement_sequence) R.(var statement * star (var semi * opt (var statement)));

      make (compoundstatement', compound_statement) R.(var kw_begin * opt (var statement_sequence) * var kw_end);

      make (u, conditional_statement) R.(var if_statement + var case_statement);

      make (ifstatement', if_statement) R.(var dotdot * var expression * var kw_then * var statement * opt (var else_part));

      make (elsepart', else_part) R.(var kw_else * var statement);

      make (casestatement', case_statement) R.(var kw_case * var case_index * var dotdot * var case_list_element *  star (var semi * var case_list_element) * opt (var semi) * var kw_end);

      make (caseelement', case_list_element) R.(var case_constant_list * var colon * var statement);

      make (u, case_index) (var expression);

      make (u, repetitive_statement) R.(var repeat_statement + var while_statement + var for_statement);

      make (repeatstatement', repeat_statement) R.(var kw_repeat * opt (var statement_sequence) * var kw_until * var expression);

      make (whilestatement', while_statement) R.(var kw_while * var expression * var dotdot * var statement);

      make (forstatement', for_statement) R.(var kw_for * var control_variable * var dotdot * var initial_value * (var dotdot + var kw_downto) * var final_value * var dotdot * var statement);

      make (u, control_variable) (var entire_variable);

      make (u, initial_value) (var expression);

      make (u, final_value) (var expression);

      make (withstatement', with_statement) R.(var kw_with * var record_variable_list * var dotdot * var statement);

      make (u, record_variable_list) R.(var record_variable * star (var comma * var record_variable));

      make (u, write_parameter) R.(var expression * var colon * var expression * opt (var colon * var expression));

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
      make (u, multiplying_operator) R.(codes "*/" * text "div" * text "mod" * text "and");
      make (u, adding_operator) R.(codes "+-" * text "or");
      make (u, relational_operator) R.(codes "=<>" * text "<>" * text ">=" * text "in");

      make (u, comma) (text ",");
      make (u, semi) (text ";");
      make (u, eq) (text "=");
      make (u, lparen) (text "(");
      make (u, rparen) (text ")");
      make (u, dotdot) (text "..");
      make (u, colon) (text ":");
      make (u, dot) (text ".");
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

      make (u, ws) R.(star (codes " \n\r\t"));
    ]

end)

let _ =
  let d = X.driver () in
  X.Run.file (fun c -> d#read c) "test.pas";
  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))

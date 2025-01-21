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

      make (u, label_declaration_part) R.(text "label" * var label * star (text "," * var label) * text ";");

      make (u, constant_definition_part) R.((text "const" * var constant_definition * text ";") * star (var constant_definition * text ";"));

      make (u, type_definition_part) R.((text "type" * var type_definition * text ";") * star (var type_definition * text ";"));

      make (u, variable_declaration_part) R.((text "var" * var variable_declaration * text ";") * star (var variable_declaration * text ";"));

      make (u, procedure_and_function_declaration_part) R.((var procedure_declaration + var function_declaration) * text ";");

      make (u, statement_part) (var compound_statement);

      make (constantdefinition', constant_definition) R.(var identifier * text "=" * var constant);

      make (u, constant) R.(var sign * (var unsigned_number + var identifier) + var unsigned_number + var identifier + var character_string);

      make (typedefinition', type_definition) R.(var identifier * text "=" * var type_denoter);

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

      make (enumeratedtype', enumerated_type) R.(text "(" * var identifier_list * text ")");

      make (u, identifier_list) R.(var identifier * star (text "," * var identifier));

      make (subrangetype', subrange_type) R.(var constant * text ".." * var constant);

      make (structuredtype', structured_type) R.(var new_structured_type + var identifier);

      make (u, new_structured_type) R.(opt (text "packed") * var unpacked_structured_type);

      make (u, unpacked_structured_type) R.(var array_type + var record_type + var set_type + var file_type);

      make (arraytype', array_type) R.(text "array" * text "[" * var index_type * star (text "," * var index_type) * text "]" * text "of" * var component_type);

      make (u, index_type) (var ordinal_type);

      make (u, component_type) (var type_denoter);

      make (recordtype', record_type) R.(text "record" * opt (var field_list) * text "end");

      make (u, field_list) R.(var fixed_part * opt (text ";" * var variant_part) + var variant_part);

      make (u, fixed_part) R.(var fixed_part * text ";" * var record_section + var record_section);

      make (u, record_section) R.(var identifier_list * text ":" * var type_denoter);

      make (u, variant_part) R.(text "case" * var variant_selector * text "of" * var variant * star (text ";" * var variant));

      make (u, variant_selector) R.(var tag_field * text ":" * var tag_type + var tag_type);

      make (u, tag_field) (var identifier);

      make (variant', variant) R.(var case_constant_list * text ":" * text "(" * var field_list * text ")");

      make (u, tag_type) (var identifier);

      make (u, case_constant_list) R.(var case_constant * star (text "," * var case_constant));

      make (u, case_constant) (var constant);

      make (settype', set_type) R.(text "set" * text "of" * var base_type);

      make (u, base_type) (var ordinal_type);

      make (filetype', file_type) R.(text "file" * text "of" * var component_type);

      make (pointertype', pointer_type) R.(var new_pointer_type + var identifier);

      make (u, new_pointer_type) R.(text "^" * var domain_type);

      make (u, domain_type) (var identifier);

      make (variabledeclaration', variable_declaration) R.(var identifier_list * text ":" * var type_denoter);

      make (variableaccess', variable_access) R.(var identified_variable + var indexed_variable + var record_variable * text "." * var field_specifier + var identifier);

      make (entirevariable', entire_variable) (var identifier);

      make (identifiedvariable', identified_variable) R.(var pointer_variable * text "^");

      make (pointervariable', pointer_variable) (var variable_access);

      make (indexedvariable', indexed_variable) R.(var array_variable * text "[" * var index_expression * star (text "," * var index_expression) * text "]");

      make (arrayvariable', array_variable) (var variable_access);

      make (u, index_expression) (var expression);

      make (recordvariable', record_variable) (var variable_access);

      make (u, field_specifier) (var identifier);

      make (proceduredeclaration', procedure_declaration) R.(var procedure_heading * text ";" * var directive + var procedure_identification * text ";" * var procedure_block + var procedure_heading * text ";" * var procedure_block);

      make (u, procedure_heading) R.(text "procedure" * var identifier * var formal_parameter_list);

      make (u, procedure_identification) R.(text "procedure" * var identifier);

      make (u, procedure_block) (var block);

      make (functiondeclaration', function_declaration) R.(var function_heading * text ";" * var directive + var function_identification * text ";" * var function_block + var function_heading * text ";" * var function_block);


      make (u, function_heading) R.(text "function" * var identifier * opt (var formal_parameter_list) * text ":" * var result_type);

      make (u, function_identification) R.(text "function" * var identifier);

      make (u, result_type) (var identifier);

      make (u, function_block) (var block);

      make (parameters', formal_parameter_list) R.(text "(" * var formal_parameter_section * star (text ";" * var formal_parameter_section) * text ")");

      make (u, formal_parameter_section) R.(var value_parameter_specification + var variable_parameter_specification + var procedural_parameter_specification + var functional_parameter_specification + var conformant_array_parameter_specification);

      make (valueparameter', value_parameter_specification) R.(var identifier_list * text ":" * var identifier);

      make (variableparameter', variable_parameter_specification) R.(text "var" * var identifier_list * text ":" * var identifier);

      make (proceduralparameter', procedural_parameter_specification) (var procedure_heading);

      make (functionalparameter', functional_parameter_specification) (var function_heading);

      make (arrayparameter', conformant_array_parameter_specification) R.(var value_conformant_array_specification + var variable_conformant_array_specification);

      make (u, value_conformant_array_specification) R.(var identifier_list * text ":" * var conformant_array_schema);

      make (u, variable_conformant_array_specification) R.(text "var" * var identifier_list * text ":" * var conformant_array_schema);

      make (u, conformant_array_schema) R.(var packed_conformant_array_schema + var unpacked_conformant_array_schema);

      make (u, packed_conformant_array_schema) R.(text "packed" * text "array" * text "[" * var index_type_specification * text "]" * text "of" * var identifier);

      make (u, unpacked_conformant_array_schema) R.(text "array" * text "[" * var index_type_specification * star (text ";" * var index_type_specification) * text "]" * text "of" * (var identifier + var conformant_array_schema));

      make (u, index_type_specification) R.(var identifier * text ".." * var identifier * text ":" * var ordinal_type_identifier);

      make (expression', expression) R.(var simple_expression * star (var relational_operator * var simple_expression));

      make (simpleepression', simple_expression) R.(var sign * var unsigned_simple_expression + var unsigned_simple_expression);

      make (unsignedsimpleexpression', unsigned_simple_expression) R.(var term * star (var adding_operator * var term));

      make (term', term) R.(var factor * star (var multiplying_operator * var factor));

      make (factor', factor) R.(var variable_access + var unsigned_constant + var function_designator + var set_constructor + text "(" * var expression * text ")" + text "not" * var factor);

      make (u, unsigned_constant) R.(var unsigned_number + var character_string + text "nil");

      make (setconstructor', set_constructor) R.(text "[" * opt (var member_designator * star (text "," * var member_designator)) * text "]");

      make (u, member_designator) R.(var expression * opt (text ".." * var expression));

      make (functiondesignator', function_designator) R.(var identifier * var actual_parameter_list);

      make (u, actual_parameter_list) R.(text "(" * var actual_parameter * star (text "," * star (var actual_parameter + var write_parameter)) * text ")");

      make (actualparameter', actual_parameter) (var expression);

      make (statement', statement) R.(opt (var label * text ":") * (var simple_statement + var structured_statement));

      make (simplestatement', simple_statement) R.(var assignment_statement + var procedure_statement + var goto_statement);

      make (assignmentstatement', assignment_statement) R.(var variable_access * text ":=" * var expression);

      make (procedurestatement', procedure_statement) R.(var identifier * opt (var actual_parameter_list));

      make (gotostatement', goto_statement) R.(text "goto" * var label);

      make (structuredstatement', structured_statement) R.(var compound_statement + var conditional_statement + var repetitive_statement + var with_statement);

      make (u, statement_sequence) R.(var statement * star (text ";" * opt (var statement)));

      make (compoundstatement', compound_statement) R.(text "begin" * opt (var statement_sequence) * text "end");

      make (u, conditional_statement) R.(var if_statement + var case_statement);

      make (ifstatement', if_statement) R.(text "if" * var expression * text "then" * var statement * opt (var else_part));

      make (elsepart', else_part) R.(text "else" * var statement);

      make (casestatement', case_statement) R.(text "case" * var case_index * text "of" * var case_list_element *  star (text ";" * var case_list_element) * opt (text ";") * text "end");

      make (caseelement', case_list_element) R.(var case_constant_list * text ":" * var statement);

      make (u, case_index) (var expression);

      make (u, repetitive_statement) R.(var repeat_statement + var while_statement + var for_statement);

      make (repeatstatement', repeat_statement) R.(text "repeat" * opt (var statement_sequence) * text "until" * var expression);

      make (whilestatement', while_statement) R.(text "while" * var expression * text "do" * var statement);

      make (forstatement', for_statement) R.(text "for" * var control_variable * text ":=" * var initial_value * (text "to" + text "downto") * var final_value * text "do" * var statement);

      make (u, control_variable) (var entire_variable);

      make (u, initial_value) (var expression);

      make (u, final_value) (var expression);

      make (withstatement', with_statement) R.(text "with" * var record_variable_list * text "do" * var statement);

      make (u, record_variable_list) R.(var record_variable * star (text "," * var record_variable));

      make (u, write_parameter) R.(var expression * text ":" * var expression * opt (text ":" * var expression));

      make (program', program) R.(var program_heading * text ";" * var program_block * text ".");

      make (u, program_heading) R.(text "program" * var identifier * opt (text "(" * var program_parameter_list * text ")"));

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

      make (u, ws) R.(star (codes " \n\r\t"));
    ]

end)

let _ =
  let d = X.driver () in
  X.Run.file (fun c -> d#read c) "test.pas";
  Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))

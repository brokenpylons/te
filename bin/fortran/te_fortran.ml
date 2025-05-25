open! Te_bot
open Te_core
open Te_top
module T = Types

module X = Spec.Build(functor(Context: Spec.CONTEXT) -> struct
  open Context

  let s = variable_supply
  let (start, s) = variable s "start"
  let (ws, s) = variable s "ws"
  let (newline, s) = variable s "newline"
  let (eos, s) = variable s "eos"

  let (executable_program, s) = variable s "executable_program"
  let (program_unit, s) = variable s "program_unit"
  let (main_program, s) = variable s "main_program"
  let (program_stmt, s) = variable s "program_stmt"
  let (lbl_def, s) = variable s "lbl_def"
  let (program_name, s) = variable s "program_name"
  let (main_range, s) = variable s "main_range"
  let (body_construct, s) = variable s "body_construct"
  let (specification_part_construct, s) = variable s "specification_part_construct"
  let (implicit_stmt, s) = variable s "implicit_stmt"
  let (implicit_spec, s) = variable s "implicit_spec"
  let (type_spec, s) = variable s "type_spec"
  let (kind_selector, s) = variable s "kind_selector"
  let (expr, s) = variable s "expr"
  let (defined_binary_op, s) = variable s "defined_binary_op"
  let (level_5_expr, s) = variable s "level_5_expr"
  let (equiv_op, s) = variable s "equiv_op"
  let (equiv_operand, s) = variable s "equiv_operand"
  let (or_op, s) = variable s "or_op"
  let (or_operand, s) = variable s "or_operand"
  let (and_op, s) = variable s "and_op"
  let (and_operand, s) = variable s "and_operand"
  let (not_op, s) = variable s "not_op"
  let (level_4_expr, s) = variable s "level_4_expr"
  let (level_3_expr, s) = variable s "level_3_expr"
  let (concat_op, s) = variable s "concat_op"
  let (level_2_expr, s) = variable s "level_2_expr"
  let (add_op, s) = variable s "add_op"
  let (add_operand, s) = variable s "add_operand"
  let (mult_op, s) = variable s "mult_op"
  let (mult_operand, s) = variable s "mult_operand"
  let (level_1_expr, s) = variable s "level_1_expr"
  let (defined_unary_op, s) = variable s "defined_unary_op"
  let (primary, s) = variable s "primary"
  let (array_constructor, s) = variable s "array_constructor"
  let (ac_value_list, s) = variable s "ac_value_list"
  let (ac_value, s) = variable s "ac_value"
  let (ac_implied_do, s) = variable s "ac_implied_do"
  let (implied_do_variable, s) = variable s "implied_do_variable"
  let (unsigned_arithmetic_constant, s) = variable s "unsigned_arithmetic_constant"
  let (complex_const, s) = variable s "complex_const"
  let (name, s) = variable s "name"
  let (data_ref, s) = variable s "data_ref"
  let (section_subscript_list, s) = variable s "section_subscript_list"
  let (section_subscript, s) = variable s "section_subscript"
  let (subscript_triplet, s) = variable s "subscript_triplet"
  let (function_reference, s) = variable s "function_reference"
  let (function_arg_list, s) = variable s "function_arg_list"
  let (function_arg, s) = variable s "function_arg"
  let (power_op, s) = variable s "power_op"
  let (rel_op, s) = variable s "rel_op"
  let (char_selector, s) = variable s "char_selector"
  let (type_param_value, s) = variable s "type_param_value"
  let (specification_expr, s) = variable s "specification_expr"
  let (type_name, s) = variable s "type_name"
  let (length_selector, s) = variable s "length_selector"
  let (char_length, s) = variable s "char_length"
  let (letter_spec, s) = variable s "letter_spec"
  let (parameter_stmt, s) = variable s "parameter_stmt"
  let (named_constant_def_list, s) = variable s "named_constant_def_list"
  let (named_constant_def, s) = variable s "named_constant_def"
  let (named_constant, s) = variable s "named_constant"
  let (format_stmt, s) = variable s "format_stmt"
  let (format_item_list, s) = variable s "format_item_list"
  let (format_item, s) = variable s "format_item"
  let (data_edit_descr, s) = variable s "data_edit_descr"
  let (control_edit_descr, s) = variable s "control_edit_descr"
  let (position_edit_descr, s) = variable s "position_edit_descr"
  let (sign_edit_descr, s) = variable s "sign_edit_descr"
  let (blank_interp_edit_descr, s) = variable s "blank_interp_edit_descr"
  let (entry_stmt, s) = variable s "entry_stmt"
  let (entry_name, s) = variable s "entry_name"
  let (subroutine_par_list, s) = variable s "subroutine_par_list"
  let (subroutine_par, s) = variable s "subroutine_par"
  let (dummy_arg_name, s) = variable s "dummy_arg_name"
  let (declaration_construct, s) = variable s "declaration_construct"
  let (type_declaration_stmt, s) = variable s "type_declaration_stmt"
  let (attr_spec, s) = variable s "attr_spec"
  let (access_spec, s) = variable s "access_spec"
  let (array_spec, s) = variable s "array_spec"
  let (assumed_shape_spec_list, s) = variable s "assumed_shape_spec_list"
  let (lower_bound, s) = variable s "lower_bound"
  let (deferred_shape_spec_list, s) = variable s "deferred_shape_spec_list"
  let (deferred_shape_spec, s) = variable s "deferred_shape_spec"
  let (assumed_shape_spec, s) = variable s "assumed_shape_spec"
  let (explicit_shape_spec_list, s) = variable s "explicit_shape_spec_list"
  let (explicit_shape_spec, s) = variable s "explicit_shape_spec"
  let (upper_bound, s) = variable s "upper_bound"
  let (assumed_size_spec, s) = variable s "assumed_size_spec"
  let (intent_spec, s) = variable s "intent_spec"
  let (entity_decl, s) = variable s "entity_decl"
  let (object_name, s) = variable s "object_name"
  let (specification_stmt, s) = variable s "specification_stmt"
  let (access_stmt, s) = variable s "access_stmt"
  let (access_id_list, s) = variable s "access_id_list"
  let (access_id, s) = variable s "access_id"
  let (generic_name, s) = variable s "generic_name"
  let (generic_spec, s) = variable s "generic_spec"
  let (defined_operator, s) = variable s "defined_operator"
  let (allocatable_stmt, s) = variable s "allocatable_stmt"
  let (array_allocation_list, s) = variable s "array_allocation_list"
  let (array_allocation, s) = variable s "array_allocation"
  let (array_name, s) = variable s "array_name"
  let (common_stmt, s) = variable s "common_stmt"
  let (comblock, s) = variable s "comblock"
  let (common_block_name, s) = variable s "common_block_name"
  let (common_block_object_list, s) = variable s "common_block_object_list"
  let (common_block_object, s) = variable s "common_block_object"
  let (variable_name, s) = variable s "variable_name"
  let (array_declarator, s) = variable s "array_declarator"
  let (data_stmt, s) = variable s "data_stmt"
  let (datalist, s) = variable s "datalist"
  let (data_stmt_set, s) = variable s "data_stmt_set"
  let (data_stmt_object_list, s) = variable s "data_stmt_object_list"
  let (data_stmt_object, s) = variable s "data_stmt_object"
  let (variable_, s) = variable s "variable_"
  let (subscript, s) = variable s "subscript"
  let (substring_range, s) = variable s "substring_range"
  let (data_implied_do, s) = variable s "data_implied_do"
  let (data_do_object_list, s) = variable s "data_do_object_list"
  let (data_do_object, s) = variable s "data_do_object"
  let (array_element, s) = variable s "array_element"
  let (structure_component, s) = variable s "structure_component"
  let (field_selector, s) = variable s "field_selector"
  let (data_stmt_value_list, s) = variable s "data_stmt_value_list"
  let (data_stmt_value, s) = variable s "data_stmt_value"
  let (constant, s) = variable s "constant"
  let (structure_constructor, s) = variable s "structure_constructor"
  let (boz_literal_constant, s) = variable s "boz_literal_constant"
  let (dimension_stmt, s) = variable s "dimension_stmt"
  let (array_declarator_list, s) = variable s "array_declarator_list"
  let (equivalence_stmt, s) = variable s "equivalence_stmt"
  let (equivalence_set_list, s) = variable s "equivalence_set_list"
  let (equivalence_set, s) = variable s "equivalence_set"
  let (equivalence_object, s) = variable s "equivalence_object"
  let (external_stmt, s) = variable s "external_stmt"
  let (external_name, s) = variable s "external_name"
  let (intrinsic_stmt, s) = variable s "intrinsic_stmt"
  let (intrinsic_procedure_name, s) = variable s "intrinsic_procedure_name"
  let (save_stmt, s) = variable s "save_stmt"
  let (saved_entity_list, s) = variable s "saved_entity_list"
  let (saved_entity, s) = variable s "saved_entity"
  let (intent_stmt, s) = variable s "intent_stmt"
  let (intent_par_list, s) = variable s "intent_par_list"
  let (intent_par, s) = variable s "intent_par"
  let (namelist_stmt, s) = variable s "namelist_stmt"
  let (namelist_group, s) = variable s "namelist_group"
  let (namelist_group_name, s) = variable s "namelist_group_name"
  let (namelist_group_object, s) = variable s "namelist_group_object"
  let (optional_stmt, s) = variable s "optional_stmt"
  let (optional_par_list, s) = variable s "optional_par_list"
  let (optional_par, s) = variable s "optional_par"
  let (pointer_stmt, s) = variable s "pointer_stmt"
  let (pointer_stmt_object_list, s) = variable s "pointer_stmt_object_list"
  let (pointer_stmt_object, s) = variable s "pointer_stmt_object"
  let (target_stmt, s) = variable s "target_stmt"
  let (target_object_list, s) = variable s "target_object_list"
  let (target_object, s) = variable s "target_object"
  let (derived_type_def, s) = variable s "derived_type_def"
  let (derived_type_stmt, s) = variable s "derived_type_stmt"
  let (derived_type_body, s) = variable s "derived_type_body"
  let (private_sequence_stmt, s) = variable s "private_sequence_stmt"
  let (component_def_stmt, s) = variable s "component_def_stmt"
  let (component_attr_spec_list, s) = variable s "component_attr_spec_list"
  let (component_attr_spec, s) = variable s "component_attr_spec"
  let (component_array_spec, s) = variable s "component_array_spec"
  let (component_decl_list, s) = variable s "component_decl_list"
  let (component_decl, s) = variable s "component_decl"
  let (component_name, s) = variable s "component_name"
  let (end_type_stmt, s) = variable s "end_type_stmt"
  let (interface_block, s) = variable s "interface_block"
  let (interface_stmt, s) = variable s "interface_stmt"
  let (interface_block_part, s) = variable s "interface_block_part"
  let (interface_body, s) = variable s "interface_body"
  let (function_prefix, s) = variable s "function_prefix"
  let (function_name, s) = variable s "function_name"
  let (function_interface_range, s) = variable s "function_interface_range"
  let (function_par_list, s) = variable s "function_par_list"
  let (function_par, s) = variable s "function_par"
  let (subprogram_interface_body, s) = variable s "subprogram_interface_body"
  let (end_function_stmt, s) = variable s "end_function_stmt"
  let (end_name, s) = variable s "end_name"
  let (subroutine_name, s) = variable s "subroutine_name"
  let (subroutine_interface_range, s) = variable s "subroutine_interface_range"
  let (end_subroutine_stmt, s) = variable s "end_subroutine_stmt"
  let (module_procedure_stmt, s) = variable s "module_procedure_stmt"
  let (procedure_name_list, s) = variable s "procedure_name_list"
  let (procedure_name, s) = variable s "procedure_name"
  let (end_interface_stmt, s) = variable s "end_interface_stmt"
  let (use_stmt, s) = variable s "use_stmt"
  let (rename_list, s) = variable s "rename_list"
  let (rename, s) = variable s "rename"
  let (use_name, s) = variable s "use_name"
  let (only_list, s) = variable s "only_list"
  let (only, s) = variable s "only"
  let (executable_construct, s) = variable s "executable_construct"
  let (action_stmt, s) = variable s "action_stmt"
  let (allocate_stmt, s) = variable s "allocate_stmt"
  let (allocation_list, s) = variable s "allocation_list"
  let (allocation, s) = variable s "allocation"
  let (allocate_object, s) = variable s "allocate_object"
  let (allocated_shape, s) = variable s "allocated_shape"
  let (cycle_stmt, s) = variable s "cycle_stmt"
  let (deallocate_stmt, s) = variable s "deallocate_stmt"
  let (allocate_object_list, s) = variable s "allocate_object_list"
  let (exit_stmt, s) = variable s "exit_stmt"
  let (nullify_stmt, s) = variable s "nullify_stmt"
  let (pointer_object_list, s) = variable s "pointer_object_list"
  let (pointer_object, s) = variable s "pointer_object"
  let (pointer_field, s) = variable s "pointer_field"
  let (sf_expr_list, s) = variable s "sf_expr_list"
  let (sf_expr, s) = variable s "sf_expr"
  let (sf_term, s) = variable s "sf_term"
  let (sf_factor, s) = variable s "sf_factor"
  let (sf_primary, s) = variable s "sf_primary"
  let (sf_dummy_arg_name_list, s) = variable s "sf_dummy_arg_name_list"
  let (sf_dummy_arg_name, s) = variable s "sf_dummy_arg_name"
  let (pointer_assignment_stmt, s) = variable s "pointer_assignment_stmt"
  let (target, s) = variable s "target"
  let (where_stmt, s) = variable s "where_stmt"
  let (mask_expr, s) = variable s "mask_expr"
  let (assignment_stmt, s) = variable s "assignment_stmt"
  let (arithmetic_if_stmt, s) = variable s "arithmetic_if_stmt"
  let (scalar_numeric_expr, s) = variable s "scalar_numeric_expr"
  let (lbl_ref, s) = variable s "lbl_ref"
  let (assign_stmt, s) = variable s "assign_stmt"
  let (backspace_stmt, s) = variable s "backspace_stmt"
  let (unit_identifier, s) = variable s "unit_identifier"
  let (uf_expr, s) = variable s "uf_expr"
  let (uf_term, s) = variable s "uf_term"
  let (uf_factor, s) = variable s "uf_factor"
  let (uf_primary, s) = variable s "uf_primary"
  let (position_spec, s) = variable s "position_spec"
  let (scalar_variable, s) = variable s "scalar_variable"
  let (call_stmt, s) = variable s "call_stmt"
  let (subroutine_name_use, s) = variable s "subroutine_name_use"
  let (actual_arg, s) = variable s "actual_arg"
  let (close_stmt, s) = variable s "close_stmt"
  let (close_spec_list, s) = variable s "close_spec_list"
  let (close_spec, s) = variable s "close_spec"
  let (char_expr, s) = variable s "char_expr"
  let (char_primary, s) = variable s "char_primary"
  let (char_operand, s) = variable s "char_operand"
  let (continue_stmt, s) = variable s "continue_stmt"
  let (endfile_stmt, s) = variable s "endfile_stmt"
  let (goto_stmt, s) = variable s "goto_stmt"
  let (go_to_kw, s) = variable s "go_to_kw"
  let (computed_goto_stmt, s) = variable s "computed_goto_stmt"
  let (scalar_int_expr, s) = variable s "scalar_int_expr"
  let (assigned_goto_stmt, s) = variable s "assigned_goto_stmt"
  let (if_stmt, s) = variable s "if_stmt"
  let (scalar_logical_expr, s) = variable s "scalar_logical_expr"
  let (inquire_stmt, s) = variable s "inquire_stmt"
  let (inquire_spec_list, s) = variable s "inquire_spec_list"
  let (inquire_spec, s) = variable s "inquire_spec"
  let (output_item_list, s) = variable s "output_item_list"
  let (output_item, s) = variable s "output_item"
  let (output_implied_do, s) = variable s "output_implied_do"
  let (open_stmt, s) = variable s "open_stmt"
  let (connect_spec_list, s) = variable s "connect_spec_list"
  let (connect_spec, s) = variable s "connect_spec"
  let (pause_stmt, s) = variable s "pause_stmt"
  let (print_stmt, s) = variable s "print_stmt"
  let (format_identifier, s) = variable s "format_identifier"
  let (read_stmt, s) = variable s "read_stmt"
  let (rd_ctl_spec, s) = variable s "rd_ctl_spec"
  let (rd_unit_id, s) = variable s "rd_unit_id"
  let (rd_io_ctl_spec_list, s) = variable s "rd_io_ctl_spec_list"
  let (io_control_spec, s) = variable s "io_control_spec"
  let (input_item_list, s) = variable s "input_item_list"
  let (input_item, s) = variable s "input_item"
  let (input_implied_do, s) = variable s "input_implied_do"
  let (rd_fmt_id, s) = variable s "rd_fmt_id"
  let (rd_fmt_id_expr, s) = variable s "rd_fmt_id_expr"
  let (return_stmt, s) = variable s "return_stmt"
  let (rewind_stmt, s) = variable s "rewind_stmt"
  let (stop_stmt, s) = variable s "stop_stmt"
  let (write_stmt, s) = variable s "write_stmt"
  let (io_control_spec_list, s) = variable s "io_control_spec_list"
  let (do_construct, s) = variable s "do_construct"
  let (block_do_construct, s) = variable s "block_do_construct"
  let (loop_control, s) = variable s "loop_control"
  let (label_do_stmt, s) = variable s "label_do_stmt"
  let (if_construct, s) = variable s "if_construct"
  let (if_then_stmt, s) = variable s "if_then_stmt"
  let (if_construct_name, s) = variable s "if_construct_name"
  let (execution_part_construct, s) = variable s "execution_part_construct"
  let (else_if_stmt, s) = variable s "else_if_stmt"
  let (else_stmt, s) = variable s "else_stmt"
  let (end_if_stmt, s) = variable s "end_if_stmt"
  let (case_construct, s) = variable s "case_construct"
  let (select_case_range, s) = variable s "select_case_range"
  let (select_case_body, s) = variable s "select_case_body"
  let (case_body_construct, s) = variable s "case_body_construct"
  let (case_stmt, s) = variable s "case_stmt"
  let (case_selector, s) = variable s "case_selector"
  let (case_value_range, s) = variable s "case_value_range"
  let (end_select_stmt, s) = variable s "end_select_stmt"
  let (where_construct, s) = variable s "where_construct"
  let (where_construct_stmt, s) = variable s "where_construct_stmt"
  let (elsewhere_stmt, s) = variable s "elsewhere_stmt"
  let (end_where_stmt, s) = variable s "end_where_stmt"
  let (end_do_stmt, s) = variable s "end_do_stmt"
  let (end_program_stmt, s) = variable s "end_program_stmt"
  let (internal_sub_prog_part, s) = variable s "internal_sub_prog_part"
  let (body, s) = variable s "body"
  let (contains_stmt, s) = variable s "contains_stmt"
  let (internal_subprogram, s) = variable s "internal_subprogram"
  let (function_subprogram, s) = variable s "function_subprogram"
  let (function_range, s) = variable s "function_range"
  let (subroutine_subprogram, s) = variable s "subroutine_subprogram"
  let (subroutine_range, s) = variable s "subroutine_range"
  let (module_, s) = variable s "module_"
  let (module_stmt, s) = variable s "module_stmt"
  let (module_name, s) = variable s "module_name"
  let (module_body, s) = variable s "module_body"
  let (module_subprogram_part_construct, s) = variable s "module_subprogram_part_construct"
  let (module_subprogram, s) = variable s "module_subprogram"
  let (end_module_stmt, s) = variable s "end_module_stmt"
  let (block_data_subprogram, s) = variable s "block_data_subprogram"
  let (block_data_stmt, s) = variable s "block_data_stmt"
  let (block_data_name, s) = variable s "block_data_name"
  let (block_data_body, s) = variable s "block_data_body"
  let (block_data_body_construct, s) = variable s "block_data_body_construct"
  let (end_block_data_stmt, s) = variable s "end_block_data_stmt"

  let (ident, s) = variable s "ident"
  let (sign, s) = variable s "sign"
  let (letter, s) = variable s "letter"
  let (int_literal_constant, s) = variable s "int_literal_constant"
  let (scalar_int_literal_constant, s) = variable s "scalar_int_literal_constant"
  let (real_literal_constant, s) = variable s "real_literal_constant"
  let (char_literal_constant, s) = variable s "char_literal_constant"
  let (hex_constant, s) = variable s "hex_constant"
  let (octal_constant, s) = variable s "octal_constant"
  let (binary_constant, s) = variable s "binary_constant"
  let (logical_constant, s) = variable s "logical_constant"
  let (char_string_edit_descr, s) = variable s "char_string_edit_descr"
  let (dop, s) = variable s "dop"
  let (label, s) = variable s "label"
  let (perc, s) = variable s "perc"
  let (lparen, s) = variable s "lparen"
  let (larray, s) = variable s "larray"
  let (rparen, s) = variable s "rparen"
  let (ast, s) = variable s "ast"
  let (astast, s) = variable s "astast"
  let (plus_, s) = variable s "plus_"
  let (comma, s) = variable s "comma"
  let (minus, s) = variable s "minus"
  let (dot, s) = variable s "dot"
  let (slash, s) = variable s "slash"
  let (rarray, s) = variable s "rarray"
  let (slashslash, s) = variable s "slashslash"
  let (slasheq, s) = variable s "slasheq"
  let (colon, s) = variable s "colon"
  let (coloncolon, s) = variable s "coloncolon"
  let (lt, s) = variable s "lt"
  let (le, s) = variable s "le"
  let (assign, s) = variable s "assign"
  let (eq, s) = variable s "eq"
  let (arrow, s) = variable s "arrow"
  let (gt, s) = variable s "gt"
  let (ge, s) = variable s "ge"
  let (a_kw, s) = variable s "a_kw"
  let (access_kw, s) = variable s "access_kw"
  let (action_kw, s) = variable s "action_kw"
  let (advance_kw, s) = variable s "advance_kw"
  let (allocatable_kw, s) = variable s "allocatable_kw"
  let (allocate_kw, s) = variable s "allocate_kw"
  let (and_op_kw, s) = variable s "and_op_kw"
  let (assign_kw, s) = variable s "assign_kw"
  let (assignment_kw, s) = variable s "assignment_kw"
  let (b_kw, s) = variable s "b_kw"
  let (backspace_kw, s) = variable s "backspace_kw"
  let (blank_kw, s) = variable s "blank_kw"
  let (block_kw, s) = variable s "block_kw"
  let (bn_kw, s) = variable s "bn_kw"
  let (bz_kw, s) = variable s "bz_kw"
  let (call_kw, s) = variable s "call_kw"
  let (case_kw, s) = variable s "case_kw"
  let (character_kw, s) = variable s "character_kw"
  let (close_kw, s) = variable s "close_kw"
  let (common_kw, s) = variable s "common_kw"
  let (complex_kw, s) = variable s "complex_kw"
  let (contains_kw, s) = variable s "contains_kw"
  let (continue_kw, s) = variable s "continue_kw"
  let (cycle_kw, s) = variable s "cycle_kw"
  let (d_kw, s) = variable s "d_kw"
  let (data_kw, s) = variable s "data_kw"
  let (deallocate_kw, s) = variable s "deallocate_kw"
  let (default_kw, s) = variable s "default_kw"
  let (delim_kw, s) = variable s "delim_kw"
  let (dimension_kw, s) = variable s "dimension_kw"
  let (direct_kw, s) = variable s "direct_kw"
  let (do_kw, s) = variable s "do_kw"
  let (double_kw, s) = variable s "double_kw"
  let (e_kw, s) = variable s "e_kw"
  let (else_kw, s) = variable s "else_kw"
  let (elsewhere_kw, s) = variable s "elsewhere_kw"
  let (en_kw, s) = variable s "en_kw"
  let (end_kw, s) = variable s "end_kw"
  let (entry_kw, s) = variable s "entry_kw"
  let (eor_kw, s) = variable s "eor_kw"
  let (eq_op_kw, s) = variable s "eq_op_kw"
  let (equivalence_kw, s) = variable s "equivalence_kw"
  let (eqv_op_kw, s) = variable s "eqv_op_kw"
  let (err_kw, s) = variable s "err_kw"
  let (err_eq_kw, s) = variable s "err_eq_kw"
  let (es_kw, s) = variable s "es_kw"
  let (exist_kw, s) = variable s "exist_kw"
  let (exit_kw, s) = variable s "exit_kw"
  let (external_kw, s) = variable s "external_kw"
  let (f_kw, s) = variable s "f_kw"
  let (file_kw, s) = variable s "file_kw"
  let (fmt_kw, s) = variable s "fmt_kw"
  let (form_kw, s) = variable s "form_kw"
  let (format_kw, s) = variable s "format_kw"
  let (formatted_kw, s) = variable s "formatted_kw"
  let (function_kw, s) = variable s "function_kw"
  let (g_kw, s) = variable s "g_kw"
  let (ge_op_kw, s) = variable s "ge_op_kw"
  let (go_kw, s) = variable s "go_kw"
  let (gt_op_kw, s) = variable s "gt_op_kw"
  let (i_kw, s) = variable s "i_kw"
  let (if_kw, s) = variable s "if_kw"
  let (implicit_kw, s) = variable s "implicit_kw"
  let (in_kw, s) = variable s "in_kw"
  let (inquire_kw, s) = variable s "inquire_kw"
  let (integer_kw, s) = variable s "integer_kw"
  let (intent_kw, s) = variable s "intent_kw"
  let (interface_kw, s) = variable s "interface_kw"
  let (intrinsic_kw, s) = variable s "intrinsic_kw"
  let (iolength_kw, s) = variable s "iolength_kw"
  let (iostat_kw, s) = variable s "iostat_kw"
  let (iostat_eq_kw, s) = variable s "iostat_eq_kw"
  let (kind_kw, s) = variable s "kind_kw"
  let (l_kw, s) = variable s "l_kw"
  let (le_op_kw, s) = variable s "le_op_kw"
  let (len_kw, s) = variable s "len_kw"
  let (logical_kw, s) = variable s "logical_kw"
  let (lt_op_kw, s) = variable s "lt_op_kw"
  let (module_kw, s) = variable s "module_kw"
  let (name_kw, s) = variable s "name_kw"
  let (named_kw, s) = variable s "named_kw"
  let (namelist_kw, s) = variable s "namelist_kw"
  let (ne_op_kw, s) = variable s "ne_op_kw"
  let (neqv_op_kw, s) = variable s "neqv_op_kw"
  let (nextrec_kw, s) = variable s "nextrec_kw"
  let (nml_kw, s) = variable s "nml_kw"
  let (none_kw, s) = variable s "none_kw"
  let (not_op_kw, s) = variable s "not_op_kw"
  let (nullify_kw, s) = variable s "nullify_kw"
  let (number_kw, s) = variable s "number_kw"
  let (o_kw, s) = variable s "o_kw"
  let (only_kw, s) = variable s "only_kw"
  let (open_kw, s) = variable s "open_kw"
  let (opened_kw, s) = variable s "opened_kw"
  let (operator_kw, s) = variable s "operator_kw"
  let (optional_kw, s) = variable s "optional_kw"
  let (or_op_kw, s) = variable s "or_op_kw"
  let (out_kw, s) = variable s "out_kw"
  let (p_kw, s) = variable s "p_kw"
  let (pad_kw, s) = variable s "pad_kw"
  let (parameter_kw, s) = variable s "parameter_kw"
  let (pause_kw, s) = variable s "pause_kw"
  let (pointer_kw, s) = variable s "pointer_kw"
  let (position_kw, s) = variable s "position_kw"
  let (precision_kw, s) = variable s "precision_kw"
  let (print_kw, s) = variable s "print_kw"
  let (private_kw, s) = variable s "private_kw"
  let (procedure_kw, s) = variable s "procedure_kw"
  let (program_kw, s) = variable s "program_kw"
  let (public_kw, s) = variable s "public_kw"
  let (read_kw, s) = variable s "read_kw"
  let (readwrite_kw, s) = variable s "readwrite_kw"
  let (real_kw, s) = variable s "real_kw"
  let (rec_kw, s) = variable s "rec_kw"
  let (recl_kw, s) = variable s "recl_kw"
  let (recursive_kw, s) = variable s "recursive_kw"
  let (result_kw, s) = variable s "result_kw"
  let (return_kw, s) = variable s "return_kw"
  let (rewind_kw, s) = variable s "rewind_kw"
  let (s_kw, s) = variable s "s_kw"
  let (save_kw, s) = variable s "save_kw"
  let (select_kw, s) = variable s "select_kw"
  let (sequence_kw, s) = variable s "sequence_kw"
  let (sequential_kw, s) = variable s "sequential_kw"
  let (size_kw, s) = variable s "size_kw"
  let (sp_kw, s) = variable s "sp_kw"
  let (ss_kw, s) = variable s "ss_kw"
  let (stat_kw, s) = variable s "stat_kw"
  let (status_kw, s) = variable s "status_kw"
  let (stop_kw, s) = variable s "stop_kw"
  let (subroutine_kw, s) = variable s "subroutine_kw"
  let (t_kw, s) = variable s "t_kw"
  let (target_kw, s) = variable s "target_kw"
  let (then_kw, s) = variable s "then_kw"
  let (tl_kw, s) = variable s "tl_kw"
  let (to_kw, s) = variable s "to_kw"
  let (tr_kw, s) = variable s "tr_kw"
  let (type_kw, s) = variable s "type_kw"
  let (unformatted_kw, s) = variable s "unformatted_kw"
  let (unit_kw, s) = variable s "unit_kw"
  let (unit_eq_kw, s) = variable s "unit_eq_kw"
  let (use_kw, s) = variable s "use_kw"
  let (where_kw, s) = variable s "where_kw"
  let (while_kw, s) = variable s "while_kw"
  let (write_kw, s) = variable s "write_kw"
  let (x_kw, s) = variable s "x_kw"
  let (z_kw, s) = variable s "z_kw"

  let (u, s) = variable s "u"

  let syntactic = [
    start;
    eos;
    executable_program;
    program_unit;
    main_program;
    program_stmt;
    lbl_def;
    program_name;
    main_range;
    body_construct;
    specification_part_construct;
    implicit_stmt;
    implicit_spec;
    type_spec;
    kind_selector;
    expr;
    defined_binary_op;
    level_5_expr;
    equiv_op;
    equiv_operand;
    or_op;
    or_operand;
    and_op;
    and_operand;
    not_op;
    level_4_expr;
    level_3_expr;
    concat_op;
    level_2_expr;
    add_op;
    add_operand;
    mult_op;
    mult_operand;
    level_1_expr;
    defined_unary_op;
    primary;
    array_constructor;
    ac_value_list;
    ac_value;
    ac_implied_do;
    implied_do_variable;
    unsigned_arithmetic_constant;
    complex_const;
    name;
    data_ref;
    section_subscript_list;
    section_subscript;
    subscript_triplet;
    function_reference;
    function_arg_list;
    function_arg;
    power_op;
    rel_op;
    char_selector;
    type_param_value;
    specification_expr;
    type_name;
    length_selector;
    char_length;
    letter_spec;
    parameter_stmt;
    named_constant_def_list;
    named_constant_def;
    named_constant;
    format_stmt;
    format_item_list;
    format_item;
    data_edit_descr;
    control_edit_descr;
    position_edit_descr;
    sign_edit_descr;
    blank_interp_edit_descr;
    entry_stmt;
    entry_name;
    subroutine_par_list;
    subroutine_par;
    dummy_arg_name;
    declaration_construct;
    type_declaration_stmt;
    attr_spec;
    access_spec;
    array_spec;
    assumed_shape_spec_list;
    lower_bound;
    deferred_shape_spec_list;
    deferred_shape_spec;
    assumed_shape_spec;
    explicit_shape_spec_list;
    explicit_shape_spec;
    upper_bound;
    assumed_size_spec;
    intent_spec;
    entity_decl;
    object_name;
    specification_stmt;
    access_stmt;
    access_id_list;
    access_id;
    generic_name;
    generic_spec;
    defined_operator;
    allocatable_stmt;
    array_allocation_list;
    array_allocation;
    array_name;
    common_stmt;
    comblock;
    common_block_name;
    common_block_object_list;
    common_block_object;
    variable_name;
    array_declarator;
    data_stmt;
    datalist;
    data_stmt_set;
    data_stmt_object_list;
    data_stmt_object;
    variable_;
    subscript;
    substring_range;
    data_implied_do;
    data_do_object_list;
    data_do_object;
    array_element;
    structure_component;
    field_selector;
    data_stmt_value_list;
    data_stmt_value;
    constant;
    structure_constructor;
    boz_literal_constant;
    dimension_stmt;
    array_declarator_list;
    equivalence_stmt;
    equivalence_set_list;
    equivalence_set;
    equivalence_object;
    external_stmt;
    external_name;
    intrinsic_stmt;
    intrinsic_procedure_name;
    save_stmt;
    saved_entity_list;
    saved_entity;
    intent_stmt;
    intent_par_list;
    intent_par;
    namelist_stmt;
    namelist_group;
    namelist_group_name;
    namelist_group_object;
    optional_stmt;
    optional_par_list;
    optional_par;
    pointer_stmt;
    pointer_stmt_object_list;
    pointer_stmt_object;
    target_stmt;
    target_object_list;
    target_object;
    derived_type_def;
    derived_type_stmt;
    derived_type_body;
    private_sequence_stmt;
    component_def_stmt;
    component_attr_spec_list;
    component_attr_spec;
    component_array_spec;
    component_decl_list;
    component_decl;
    component_name;
    end_type_stmt;
    interface_block;
    interface_stmt;
    interface_block_part;
    interface_body;
    function_prefix;
    function_name;
    function_interface_range;
    function_par_list;
    function_par;
    subprogram_interface_body;
    end_function_stmt;
    end_name;
    subroutine_name;
    subroutine_interface_range;
    end_subroutine_stmt;
    module_procedure_stmt;
    procedure_name_list;
    procedure_name;
    end_interface_stmt;
    use_stmt;
    rename_list;
    rename;
    use_name;
    only_list;
    only;
    executable_construct;
    action_stmt;
    allocate_stmt;
    allocation_list;
    allocation;
    allocate_object;
    allocated_shape;
    cycle_stmt;
    deallocate_stmt;
    allocate_object_list;
    exit_stmt;
    nullify_stmt;
    pointer_object_list;
    pointer_object;
    pointer_field;
    sf_expr_list;
    sf_expr;
    sf_term;
    sf_factor;
    sf_primary;
    sf_dummy_arg_name_list;
    sf_dummy_arg_name;
    pointer_assignment_stmt;
    target;
    where_stmt;
    mask_expr;
    assignment_stmt;
    arithmetic_if_stmt;
    scalar_numeric_expr;
    lbl_ref;
    assign_stmt;
    backspace_stmt;
    unit_identifier;
    uf_expr;
    uf_term;
    uf_factor;
    uf_primary;
    position_spec;
    scalar_variable;
    call_stmt;
    subroutine_name_use;
    actual_arg;
    close_stmt;
    close_spec_list;
    close_spec;
    char_expr;
    char_primary;
    char_operand;
    continue_stmt;
    endfile_stmt;
    goto_stmt;
    go_to_kw;
    computed_goto_stmt;
    scalar_int_expr;
    assigned_goto_stmt;
    if_stmt;
    scalar_logical_expr;
    inquire_stmt;
    inquire_spec_list;
    inquire_spec;
    output_item_list;
    output_item;
    output_implied_do;
    open_stmt;
    connect_spec_list;
    connect_spec;
    pause_stmt;
    print_stmt;
    format_identifier;
    read_stmt;
    rd_ctl_spec;
    rd_unit_id;
    rd_io_ctl_spec_list;
    io_control_spec;
    input_item_list;
    input_item;
    input_implied_do;
    rd_fmt_id;
    rd_fmt_id_expr;
    return_stmt;
    rewind_stmt;
    stop_stmt;
    write_stmt;
    io_control_spec_list;
    do_construct;
    block_do_construct;
    loop_control;
    label_do_stmt;
    if_construct;
    if_then_stmt;
    if_construct_name;
    execution_part_construct;
    else_if_stmt;
    else_stmt;
    end_if_stmt;
    case_construct;
    select_case_range;
    select_case_body;
    case_body_construct;
    case_stmt;
    case_selector;
    case_value_range;
    end_select_stmt;
    where_construct;
    where_construct_stmt;
    elsewhere_stmt;
    end_where_stmt;
    end_do_stmt;
    end_program_stmt;
    internal_sub_prog_part;
    body;
    contains_stmt;
    internal_subprogram;
    function_subprogram;
    function_range;
    subroutine_subprogram;
    subroutine_range;
    module_;
    module_stmt;
    module_name;
    module_body;
    module_subprogram_part_construct;
    module_subprogram;
    end_module_stmt;
    block_data_subprogram;
    block_data_stmt;
    block_data_name;
    block_data_body;
    block_data_body_construct;
    end_block_data_stmt;
  ]
  let lexical = [
    ws;
    newline;
    ident;
    sign;
    letter;
    int_literal_constant;
    scalar_int_literal_constant;
    real_literal_constant;
    char_literal_constant;
    hex_constant;
    octal_constant;
    binary_constant;
    logical_constant;
    char_string_edit_descr;
    dop;
    label;
    perc;
    lparen;
    larray;
    rparen;
    ast;
    astast;
    plus_;
    comma;
    minus;
    dot;
    slash;
    rarray;
    slashslash;
    slasheq;
    colon;
    coloncolon;
    lt;
    le;
    assign;
    eq;
    arrow;
    gt;
    ge;
    a_kw;
    access_kw;
    action_kw;
    advance_kw;
    allocatable_kw;
    allocate_kw;
    and_op_kw;
    assign_kw;
    assignment_kw;
    b_kw;
    backspace_kw;
    blank_kw;
    block_kw;
    bn_kw;
    bz_kw;
    call_kw;
    case_kw;
    character_kw;
    close_kw;
    common_kw;
    complex_kw;
    contains_kw;
    continue_kw;
    cycle_kw;
    d_kw;
    data_kw;
    deallocate_kw;
    default_kw;
    delim_kw;
    dimension_kw;
    direct_kw;
    do_kw;
    double_kw;
    e_kw;
    else_kw;
    elsewhere_kw;
    en_kw;
    end_kw;
    entry_kw;
    eor_kw;
    eq_op_kw;
    equivalence_kw;
    eqv_op_kw;
    err_kw;
    err_eq_kw;
    es_kw;
    exist_kw;
    exit_kw;
    external_kw;
    f_kw;
    file_kw;
    fmt_kw;
    form_kw;
    format_kw;
    formatted_kw;
    function_kw;
    g_kw;
    ge_op_kw;
    go_kw;
    gt_op_kw;
    i_kw;
    if_kw;
    implicit_kw;
    in_kw;
    inquire_kw;
    integer_kw;
    intent_kw;
    interface_kw;
    intrinsic_kw;
    iolength_kw;
    iostat_kw;
    iostat_eq_kw;
    kind_kw;
    l_kw;
    le_op_kw;
    len_kw;
    logical_kw;
    lt_op_kw;
    module_kw;
    name_kw;
    named_kw;
    namelist_kw;
    ne_op_kw;
    neqv_op_kw;
    nextrec_kw;
    nml_kw;
    none_kw;
    not_op_kw;
    nullify_kw;
    number_kw;
    o_kw;
    only_kw;
    open_kw;
    opened_kw;
    operator_kw;
    optional_kw;
    or_op_kw;
    out_kw;
    p_kw;
    pad_kw;
    parameter_kw;
    pause_kw;
    pointer_kw;
    position_kw;
    precision_kw;
    print_kw;
    private_kw;
    procedure_kw;
    program_kw;
    public_kw;
    read_kw;
    readwrite_kw;
    real_kw;
    rec_kw;
    recl_kw;
    recursive_kw;
    result_kw;
    return_kw;
    rewind_kw;
    s_kw;
    save_kw;
    select_kw;
    sequence_kw;
    sequential_kw;
    size_kw;
    sp_kw;
    ss_kw;
    stat_kw;
    status_kw;
    stop_kw;
    subroutine_kw;
    t_kw;
    target_kw;
    then_kw;
    tl_kw;
    to_kw;
    tr_kw;
    type_kw;
    unformatted_kw;
    unit_kw;
    unit_eq_kw;
    use_kw;
    where_kw;
    while_kw;
    write_kw;
    x_kw;
    z_kw;
  ]

  let labels = [
    u
  ]

  let longest_match = [
    ws;
    eos;
    ident;
  ]

  let start = start

  let parser =
    with_ws (T.Vars.of_list lexical) (var ws) @@ unextend s u Production.[
      make (u, start) R.(var executable_program * plus eof);

      make (u, executable_program) R.(var ws * plus (var program_unit));

      make (u, program_unit) R.(var main_program + var function_subprogram + var subroutine_subprogram + var module_ + var block_data_subprogram);

      make (u, main_program) R.(opt (var program_stmt) * var main_range);

      make (u, program_stmt) R.(var lbl_def * var program_kw * var program_name * var eos);

      make (u, lbl_def) R.(opt (var label));

      make (u, program_name) (var ident);

      make (u, main_range) R.(plus (var body_construct) * var end_program_stmt + var internal_sub_prog_part * var end_program_stmt + var end_program_stmt);

      make (u, body_construct) R.(var specification_part_construct + var executable_construct);

      make (u, specification_part_construct) R.(var implicit_stmt + var parameter_stmt + var format_stmt + var entry_stmt + var declaration_construct + var use_stmt);

      make (u, implicit_stmt) R.(var lbl_def * var implicit_kw * var none_kw * var eos + var lbl_def * var implicit_kw * var implicit_spec * star (var comma * var implicit_spec) * var eos);

      make (u, implicit_spec) R.(var type_spec * var lparen * var letter_spec * star (var comma * var letter_spec) * var rparen);

      make (u, type_spec) R.(var integer_kw * var kind_selector + var real_kw * var kind_selector + var double_kw * var precision_kw + var complex_kw * var kind_selector + var character_kw * var char_selector + var logical_kw * var kind_selector + var type_kw * var lparen * var type_name * var rparen + var integer_kw + var real_kw + var complex_kw + var logical_kw + var character_kw + var character_kw * var length_selector);

      make (u, kind_selector) R.(var lparen * opt (var kind_kw * var assign) * var expr * var rparen + var ast * var int_literal_constant);

      make (u, expr) R.(opt (var expr * var defined_binary_op) * var level_5_expr);

      make (u, defined_binary_op) (var dop);

      make (u, level_5_expr) R.(opt (var level_5_expr * var equiv_op) * var equiv_operand);

      make (u, equiv_op) R.(var eqv_op_kw + var neqv_op_kw);

      make (u, equiv_operand) R.(opt (var equiv_operand * var or_op) * var or_operand);

      make (u, or_op) (var or_op_kw);

      make (u, or_operand) R.(opt (var or_operand * var and_op) * var and_operand);

      make (u, and_op) (var and_op_kw);

      make (u, and_operand) R.(opt (var not_op) * var level_4_expr);

      make (u, not_op) (var not_op_kw);

      make (u, level_4_expr) R.(opt (var level_3_expr * var rel_op) * var level_3_expr);

      make (u, level_3_expr) R.(opt (var level_3_expr * var concat_op) * var level_2_expr);

      make (u, concat_op) (var slashslash);

      make (u, level_2_expr) R.(opt (var level_2_expr * var add_op) * var add_operand + var sign * var add_operand);

      make (u, add_op) R.(var plus_ + var minus);

      make (u, add_operand) R.(opt (var add_operand * var mult_op) * var mult_operand);

      make (u, mult_op) R.(var ast + var slash);

      make (u, mult_operand) R.(var level_1_expr * opt (var power_op * var mult_operand));

      make (u, level_1_expr) R.(opt (var defined_unary_op) * var primary);

      make (u, defined_unary_op) (var dop);

      make (u, primary) R.(var array_constructor + var unsigned_arithmetic_constant + var name + var data_ref + var function_reference + var lparen * var expr * var rparen + var char_literal_constant + var logical_constant);

      make (u, array_constructor) R.(var larray * var ac_value_list * var rarray);

      make (u, ac_value_list) R.(var ac_value * star (var comma * var ac_value));

      make (u, ac_value) R.(var expr + var ac_implied_do);

      make (u, ac_implied_do) R.(var lparen * var expr * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var rparen + var lparen * var expr * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var comma * var expr * var rparen + var lparen * var ac_implied_do * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var rparen + var lparen * var ac_implied_do * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var comma * var expr * var rparen);

      make (u, implied_do_variable) (var ident);

      make (u, unsigned_arithmetic_constant) R.(var real_literal_constant + var int_literal_constant + var complex_const);

      make (u, complex_const) R.(var lparen * var expr * var comma * var expr * var rparen);

      make (u, name) (var ident);

      make (u, data_ref) R.(var name * var perc * var name + var data_ref * var perc * var name + var name * var lparen * var section_subscript_list * var rparen + var data_ref * var lparen * var section_subscript_list * var rparen);

      make (u, section_subscript_list) R.(var section_subscript * star (var comma * var section_subscript));

      make (u, section_subscript) R.(var expr + var subscript_triplet);

      make (u, subscript_triplet) R.(opt (var expr) * var colon * opt (var expr) * opt (var colon * var expr));

      make (u, function_reference) R.(var name * var lparen * opt (var function_arg_list) * var rparen);

      make (u, function_arg_list) R.(var function_arg + var function_arg_list * var comma * var function_arg + var section_subscript_list * var comma * var function_arg);

      make (u, function_arg) R.(var name * var assign * var expr);


      make (u, power_op) (var astast);

      make (u, rel_op) R.(var eq + var slasheq + var lt + var le + var gt + var ge + var eq_op_kw + var ne_op_kw + var lt_op_kw + var le_op_kw + var gt_op_kw + var ge_op_kw);

      make (u, char_selector) R.(var lparen * var len_kw * var assign * var type_param_value * var comma * var kind_kw * var assign * var expr * var rparen + var lparen * var len_kw * var assign * var type_param_value * var comma * var expr * var rparen + var lparen * var len_kw * var assign * var type_param_value * var rparen + var lparen * opt (var kind_kw * var assign) * var expr * var rparen);

      make (u, type_param_value) R.(var specification_expr + var ast);

      make (u, specification_expr) (var expr);

      make (u, type_name) (var ident);

      make (u, length_selector) R.(var lparen * var type_param_value * var rparen + var ast * var char_length);

      make (u, char_length) R.(var lparen * var type_param_value * var rparen + var scalar_int_literal_constant);

      make (u, letter_spec) R.(var letter * opt (var minus * var letter));

      make (u, parameter_stmt) R.(var lbl_def * var parameter_kw * var lparen * var named_constant_def_list * var rparen * var eos);

      make (u, named_constant_def_list) R.(var named_constant_def * star (var comma * var named_constant_def));

      make (u, named_constant_def) R.(var named_constant * var assign * var expr);

      make (u, named_constant) (var ident);

      make (u, format_stmt) R.(var lbl_def * var format_kw * var lparen * opt (var format_item_list) * var rparen * var eos);

      make (u, format_item_list) R.(var format_item * star (var comma * var format_item));

      make (u, format_item) R.(opt (var int_literal_constant) * var data_edit_descr + var control_edit_descr + var char_string_edit_descr + opt (var int_literal_constant) * var lparen * var format_item_list * var rparen + var format_item * var colon * var format_item);

      make (u, data_edit_descr) R.(var i_kw * var int_literal_constant * opt (var dot * var int_literal_constant) + var o_kw * var int_literal_constant * opt (var dot * var int_literal_constant) + var b_kw * var int_literal_constant * opt (var dot * var int_literal_constant) + var z_kw * var int_literal_constant * opt (var dot * var int_literal_constant) + var f_kw * var int_literal_constant * var dot * var int_literal_constant + var e_kw * var int_literal_constant * var dot * var int_literal_constant * opt (var e_kw * var int_literal_constant) + var en_kw * var int_literal_constant * var dot * var int_literal_constant * opt (var e_kw * var int_literal_constant) + var es_kw * var int_literal_constant * var dot * var int_literal_constant * opt (var e_kw * var int_literal_constant) + var g_kw * var int_literal_constant * var dot * var int_literal_constant * opt (var e_kw * var int_literal_constant) + var l_kw * var int_literal_constant + var a_kw * opt (var int_literal_constant) + var d_kw * var int_literal_constant * var dot * var int_literal_constant);

      make (u, control_edit_descr) R.(var position_edit_descr + opt (var int_literal_constant) * var slash + var colon + var sign_edit_descr + opt (var sign) * var int_literal_constant * var p_kw * opt (opt (var int_literal_constant) * var data_edit_descr) + var blank_interp_edit_descr);

      make (u, position_edit_descr) R.(var t_kw * var int_literal_constant + var tl_kw * var int_literal_constant + var tr_kw * var int_literal_constant + var int_literal_constant * var x_kw);

      make (u, sign_edit_descr) R.(var s_kw + var sp_kw + var ss_kw);

      make (u, blank_interp_edit_descr) R.(var bn_kw + var bz_kw);

      make (u, entry_stmt) R.(var lbl_def * var entry_kw * var entry_name * var subroutine_par_list * var eos + var lbl_def * var entry_kw * var entry_name * var subroutine_par_list * var result_kw * var lparen * var name * var rparen * var eos);

      make (u, entry_name) (var ident);

      make (u, subroutine_par_list) R.(var lparen * opt (var subroutine_par * star (var comma * var subroutine_par)) * var rparen);

      make (u, subroutine_par) R.(var dummy_arg_name + var ast);

      make (u, dummy_arg_name) (var ident);

      make (u, declaration_construct) R.(var type_declaration_stmt + var specification_stmt + var derived_type_def + var interface_block);

      make (u, type_declaration_stmt) R.(var lbl_def * var type_spec * star (var comma * var attr_spec) * var coloncolon * var entity_decl * star (var comma * var entity_decl) * var eos + var lbl_def * var type_spec * var entity_decl * star (var comma * var entity_decl) * var eos);

      make (u, attr_spec) R.(var parameter_kw + var access_spec + var allocatable_kw + var dimension_kw * var lparen * var array_spec * var rparen + var external_kw + var intent_kw * var lparen * var intent_spec * var rparen + var intrinsic_kw + var optional_kw + var pointer_kw + var save_kw + var target_kw);

      make (u, access_spec) R.(var public_kw + var private_kw);

      make (u, array_spec) R.(var assumed_shape_spec_list + var deferred_shape_spec_list + var explicit_shape_spec_list + var assumed_size_spec);

      make (u, assumed_shape_spec_list) R.(var lower_bound * var colon + var deferred_shape_spec_list * var comma * var lower_bound * var colon + var assumed_shape_spec_list * var comma * var assumed_shape_spec);

      make (u, lower_bound) (var specification_expr);

      make (u, deferred_shape_spec_list) R.(var deferred_shape_spec * star (var comma * var deferred_shape_spec));

      make (u, deferred_shape_spec) (var colon);

      make (u, assumed_shape_spec) R.(opt (var lower_bound) * var colon);

      make (u, explicit_shape_spec_list) R.(var explicit_shape_spec * star (var comma * var explicit_shape_spec));

      make (u, explicit_shape_spec) R.(opt (var lower_bound * var colon) * var upper_bound);

      make (u, upper_bound) (var specification_expr);

      make (u, assumed_size_spec) R.(opt (var lower_bound * var colon) * var ast + var explicit_shape_spec_list * var comma * var ast + var explicit_shape_spec_list * var comma * var lower_bound * var colon * var ast);

      make (u, intent_spec) R.(var in_kw + var out_kw + var in_kw * var out_kw);

      make (u, entity_decl) R.(var object_name * var assign * var expr + var object_name * var lparen * var array_spec * var rparen * var assign * var expr + var object_name * var ast * var char_length * var assign * var expr + var object_name * var ast * var char_length * var lparen * var array_spec * var rparen * var assign * var expr + var object_name + var object_name * var ast * var char_length + var object_name * var lparen * var array_spec * var rparen + var object_name * var lparen * var array_spec * var rparen * var ast * var char_length);

      make (u, object_name) (var ident);

      make (u, specification_stmt) R.(var access_stmt + var allocatable_stmt + var common_stmt + var data_stmt + var dimension_stmt + var equivalence_stmt + var external_stmt + var intrinsic_stmt + var save_stmt + var intent_stmt + var namelist_stmt + var optional_stmt + var pointer_stmt + var target_stmt);

      make (u, access_stmt) R.(var lbl_def * var access_spec * var coloncolon * var access_id_list * var eos + var lbl_def * var access_spec * opt (var access_id_list) * var eos);

      make (u, access_id_list) R.(var access_id * star (var comma * var access_id));

      make (u, access_id) R.(var generic_name + var generic_spec);

      make (u, generic_name) (var ident);

      make (u, generic_spec) R.(var operator_kw * var lparen * var defined_operator * var rparen + var assignment_kw * var lparen * var assign * var rparen);

      make (u, defined_operator) R.(var dop + var power_op + var mult_op + var add_op + var concat_op + var rel_op + var not_op + var and_op + var or_op + var equiv_op);

      make (u, allocatable_stmt) R.(var lbl_def * var allocatable_kw * var coloncolon * var array_allocation_list * var eos + var lbl_def * var allocatable_kw * var array_allocation_list * var eos);

      make (u, array_allocation_list) R.(var array_allocation * star (var comma * var array_allocation));

      make (u, array_allocation) R.(var array_name * opt (var lparen * var deferred_shape_spec_list * var rparen));

      make (u, array_name) (var ident);

      make (u, common_stmt) R.(var lbl_def * var common_kw * opt (var comblock) * var common_block_object_list * star (opt (var comma) * var comblock * var common_block_object_list) * var eos);

      make (u, comblock) R.(var slash * opt (var common_block_name) * var slash);

      make (u, common_block_name) (var ident);

      make (u, common_block_object_list) R.(var common_block_object * star (var comma * var common_block_object));

      make (u, common_block_object) R.(var variable_name + var array_declarator);

      make (u, variable_name) (var ident);

      make (u, array_declarator) R.(var variable_name * var lparen * var array_spec * var rparen);

      make (u, data_stmt) R.(var lbl_def * var data_kw * var datalist * var eos);

      make (u, datalist) R.(var data_stmt_set * star (var comma * var data_stmt_set));

      make (u, data_stmt_set) R.(var data_stmt_object_list * var slash * var data_stmt_value_list * var slash);

      make (u, data_stmt_object_list) R.(var data_stmt_object * star (var comma * var data_stmt_object));

      make (u, data_stmt_object) R.(var variable_ + var data_implied_do);

      make (u, variable_) R.(var variable_name + var variable_name * var lparen * var subscript * star (var comma * var subscript) * var rparen + var variable_name * var substring_range + var variable_name * var lparen * var subscript * star (var comma * var subscript) * var rparen * var substring_range);

      make (u, subscript) (var expr);

      make (u, substring_range) R.(var lparen * var subscript_triplet * var rparen);

      make (u, data_implied_do) R.(var lparen * var data_do_object_list * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * opt (var comma * var expr) * var rparen);

      make (u, data_do_object_list) R.(var data_do_object * star (var comma * var data_do_object));

      make (u, data_do_object) R.(var array_element + var data_implied_do + var structure_component);

      make (u, array_element) R.(var structure_component * var lparen * var section_subscript_list * var rparen + var variable_name * var lparen * var section_subscript_list * var rparen);

      make (u, structure_component) R.(var variable_name * var field_selector + var structure_component * var field_selector);

      make (u, field_selector) R.(var lparen * var section_subscript_list * var rparen * var perc * var name + var perc * var name);

      make (u, data_stmt_value_list) R.(var data_stmt_value * star (var comma * var data_stmt_value));

      make (u, data_stmt_value) R.(var constant + var ident * var ast * var constant);

      make (u, constant) R.(var ident + opt (var sign) * var unsigned_arithmetic_constant + var char_literal_constant + var logical_constant + var structure_constructor + var boz_literal_constant);

      make (u, structure_constructor) R.(var type_name * var lparen * var expr * star (var comma * var expr) * var rparen);

      make (u, boz_literal_constant) R.(var binary_constant + var octal_constant + var hex_constant);

      make (u, dimension_stmt) R.(var lbl_def * var dimension_kw * var coloncolon * var array_declarator_list * var eos + var lbl_def * var dimension_kw * var array_declarator_list * var eos);

      make (u, array_declarator_list) R.(var array_declarator * star (var comma * var array_declarator));

      make (u, equivalence_stmt) R.(var lbl_def * var equivalence_kw * var equivalence_set_list * var eos);

      make (u, equivalence_set_list) R.(var equivalence_set * star (var comma * var equivalence_set));

      make (u, equivalence_set) R.(var lparen * var equivalence_object * var comma * var equivalence_object * star (var comma * var equivalence_object) * var rparen);

      make (u, equivalence_object) R.(var array_name + var variable_);

      make (u, external_stmt) R.(var lbl_def * var external_kw * var external_name * star (var comma * var external_name) * var eos);

      make (u, external_name) (var ident);

      make (u, intrinsic_stmt) R.(var lbl_def * var intrinsic_kw * var intrinsic_procedure_name * star (var comma * var intrinsic_procedure_name) * var eos);

      make (u, intrinsic_procedure_name) (var ident);

      make (u, save_stmt) R.(var lbl_def * var save_kw * var coloncolon * var saved_entity_list * var eos + var lbl_def * var save_kw * opt (var saved_entity_list) * var eos);

      make (u, saved_entity_list) R.(var saved_entity * star (var comma * var saved_entity));

      make (u, saved_entity) R.(var variable_name + var slash * var common_block_name * var slash);

      make (u, intent_stmt) R.(var lbl_def * var intent_kw * var lparen * var intent_spec * var rparen * var coloncolon * var intent_par_list * var eos + var lbl_def * var intent_kw * var lparen * var intent_spec * var rparen * var intent_par_list * var eos);

      make (u, intent_par_list) R.(var intent_par * star (var comma * var intent_par));

      make (u, intent_par) (var dummy_arg_name);

      make (u, namelist_stmt) R.(var lbl_def * var namelist_kw * var namelist_group * star (opt (var comma) * var namelist_group) * var eos);

      make (u, namelist_group) R.(var slash * var namelist_group_name * var slash * var namelist_group_object * star (var comma * var namelist_group_object));

      make (u, namelist_group_name) (var ident);

      make (u, namelist_group_object) (var variable_name);

      make (u, optional_stmt) R.(var lbl_def * var optional_kw * var coloncolon * var optional_par_list * var eos + var lbl_def * var optional_kw * var optional_par_list * var eos);

      make (u, optional_par_list) R.(var optional_par * star (var comma * var optional_par));

      make (u, optional_par) (var dummy_arg_name);

      make (u, pointer_stmt) R.(var lbl_def * var pointer_kw * var coloncolon * var pointer_stmt_object_list * var eos + var lbl_def * var pointer_kw * var pointer_stmt_object_list * var eos);

      make (u, pointer_stmt_object_list) R.(var pointer_stmt_object * star (var comma * var pointer_stmt_object));

      make (u, pointer_stmt_object) R.(var object_name + var object_name * var lparen * var deferred_shape_spec_list * var rparen);

      make (u, target_stmt) R.(var lbl_def * var target_kw * var coloncolon * var target_object_list * var eos + var lbl_def * var target_kw * var target_object_list * var eos);

      make (u, target_object_list) R.(var target_object * star (var comma * var target_object));

      make (u, target_object) R.(var object_name + var object_name * var lparen * var array_spec * var rparen);

      make (u, derived_type_def) R.(var derived_type_stmt * plus (var derived_type_body) * var end_type_stmt);

      make (u, derived_type_stmt) R.(var lbl_def * var type_kw * var type_name * var eos + var lbl_def * var type_kw * var coloncolon * var type_name * var eos + var lbl_def * var type_kw * var comma * var access_spec * var coloncolon * var type_name * var eos);

      make (u, derived_type_body) R.(var private_sequence_stmt + var component_def_stmt);

      make (u, private_sequence_stmt) R.(var lbl_def * var private_kw * var eos + var lbl_def * var sequence_kw * var eos);

      make (u, component_def_stmt) R.(var lbl_def * var type_spec * opt (var comma * var component_attr_spec_list) * var coloncolon * var component_decl_list * var eos + var lbl_def * var type_spec * var component_decl_list * var eos);

      make (u, component_attr_spec_list) R.(var component_attr_spec * star (var comma * var component_attr_spec));

      make (u, component_attr_spec) R.(var pointer_kw + var dimension_kw * var lparen * var component_array_spec * var rparen);

      make (u, component_array_spec) R.(var explicit_shape_spec_list + var deferred_shape_spec_list);

      make (u, component_decl_list) R.(var component_decl * star (var comma * var component_decl));

      make (u, component_decl) R.(var component_name * opt (var lparen * var component_array_spec * var rparen) * opt (var ast * var char_length));

      make (u, component_name) (var ident);

      make (u, end_type_stmt) R.(var lbl_def * var end_kw * var type_kw * opt (var type_name) * var eos);

      make (u, interface_block) R.(var interface_stmt * plus (var interface_block_part) * var end_interface_stmt);

      make (u, interface_stmt) R.(var lbl_def * var interface_kw * var generic_name * var eos + var lbl_def * var interface_kw * var generic_spec * var eos + var lbl_def * var interface_kw * var eos);

      make (u, interface_block_part) R.(var interface_body + var module_procedure_stmt);

      make (u, interface_body) R.(var lbl_def * var function_prefix * var function_name * var function_interface_range + var lbl_def * var subroutine_kw * var subroutine_name * var subroutine_interface_range);

      make (u, function_prefix) R.(var recursive_kw * var function_kw + var recursive_kw * var type_spec * var function_kw + var type_spec * var recursive_kw * var function_kw + opt (var type_spec) * var function_kw);

      make (u, function_name) (var ident);

      make (u, function_interface_range) R.(var function_par_list * var eos * var subprogram_interface_body * var end_function_stmt + var function_par_list * var eos * var end_function_stmt);

      make (u, function_par_list) R.(var lparen * opt (var function_par * star (var comma * var function_par)) * var rparen);

      make (u, function_par) (var dummy_arg_name);

      make (u, subprogram_interface_body) R.(var specification_part_construct + var subprogram_interface_body * var specification_part_construct);

      make (u, end_function_stmt) R.(var lbl_def * var end_kw * var eos + var lbl_def * var end_kw * var function_kw * opt (var end_name) * var eos);

      make (u, end_name) (var ident);

      make (u, subroutine_name) (var ident);

      make (u, subroutine_interface_range) R.(var subroutine_par_list * var eos * var subprogram_interface_body * var end_subroutine_stmt + var subroutine_par_list * var eos * var end_subroutine_stmt);

      make (u, end_subroutine_stmt) R.(var lbl_def * var end_kw * var subroutine_kw * opt (var end_name) * var eos + var lbl_def * var end_kw * var eos);

      make (u, module_procedure_stmt) R.(var lbl_def * var module_kw * var procedure_kw * var procedure_name_list * var eos);

      make (u, procedure_name_list) R.(var procedure_name * star (var comma * var procedure_name));

      make (u, procedure_name) (var ident);

      make (u, end_interface_stmt) R.(var lbl_def * var end_kw * var interface_kw * var eos);

      make (u, use_stmt) R.(var lbl_def * var use_kw * var name * opt (var comma * var rename_list) * var eos + var lbl_def * var use_kw * var name * var comma * var only_kw * var colon * opt (var only_list) * var eos);

      make (u, rename_list) R.(var rename * star (var comma * var rename));

      make (u, rename) R.(var ident * var arrow * var use_name);

      make (u, use_name) (var ident);

      make (u, only_list) R.(var only * star (var comma * var only));

      make (u, only) R.(var generic_spec + opt (var ident * var arrow) * var use_name);

      make (u, executable_construct) R.(var action_stmt + var do_construct + var if_construct + var case_construct + var where_construct + var end_do_stmt);

      make (u, action_stmt) R.(var allocate_stmt + var cycle_stmt + var deallocate_stmt + var exit_stmt + var nullify_stmt + var pointer_assignment_stmt + var where_stmt + var arithmetic_if_stmt + var assignment_stmt + var assign_stmt + var backspace_stmt + var call_stmt + var close_stmt + var continue_stmt + var endfile_stmt + var goto_stmt + var computed_goto_stmt + var assigned_goto_stmt + var if_stmt + var inquire_stmt + var open_stmt + var pause_stmt + var print_stmt + var read_stmt + var return_stmt + var rewind_stmt + var stop_stmt + var write_stmt);

      make (u, allocate_stmt) R.(var lbl_def * var allocate_kw * var lparen * var allocation_list * var comma * var stat_kw * var assign * var variable_ * var rparen * var eos + var lbl_def * var allocate_kw * var lparen * var allocation_list * var rparen * var eos);

      make (u, allocation_list) R.(var allocation * star (var comma * var allocation));

      make (u, allocation) R.(var allocate_object * opt (var allocated_shape));

      make (u, allocate_object) R.(var variable_name + var allocate_object * var field_selector);

      make (u, allocated_shape) R.(var lparen * var section_subscript_list * var rparen);

      make (u, cycle_stmt) R.(var lbl_def * var cycle_kw * opt (var end_name) * var eos);

      make (u, deallocate_stmt) R.(var lbl_def * var deallocate_kw * var lparen * var allocate_object_list * var comma * var stat_kw * var assign * var variable_ * var rparen * var eos + var lbl_def * var deallocate_kw * var lparen * var allocate_object_list * var rparen * var eos);

      make (u, allocate_object_list) R.(var allocate_object * star (var comma * var allocate_object));

      make (u, exit_stmt) R.(var lbl_def * var exit_kw * opt (var end_name) * var eos);

      make (u, nullify_stmt) R.(var lbl_def * var nullify_kw * var lparen * var pointer_object_list * var rparen * var eos);

      make (u, pointer_object_list) R.(var pointer_object * star (var comma * var pointer_object));

      make (u, pointer_object) R.(var name + var pointer_field);

      make (u, pointer_field) R.(var name * var lparen * var sf_expr_list * var rparen * var perc * var name + var name * var lparen * var sf_dummy_arg_name_list * var rparen * var perc * var name + var name * var perc * var name + var pointer_field * var field_selector);

      make (u, sf_expr_list) R.(var sf_expr * var colon * var expr * var colon * var expr + var sf_expr * var colon * var colon * var expr + var colon * var expr * var colon * var expr + var colon * var colon * var expr + var colon + var colon * var expr + var sf_expr + var sf_expr * var colon + var sf_expr * var colon * var expr + var sf_expr_list * var comma * var section_subscript + var sf_dummy_arg_name_list * var comma * var colon + var sf_dummy_arg_name_list * var comma * var colon * var expr + var sf_dummy_arg_name_list * var comma * var sf_expr * var colon + var sf_dummy_arg_name_list * var comma * opt (var sf_expr) * var colon * var expr);

      make (u, sf_expr) R.(var sf_term + var sign * var add_operand + var sf_expr * var add_op * var add_operand);

      make (u, sf_term) R.(var sf_factor + var sf_term * var mult_op * var mult_operand);

      make (u, sf_factor) R.(var sf_primary + var sf_primary * var power_op * var mult_operand);

      make (u, sf_primary) R.(var array_constructor + var int_literal_constant + var name + var data_ref + var function_reference + var lparen * var expr * var rparen);

      make (u, sf_dummy_arg_name_list) R.(var sf_dummy_arg_name * star (var comma * var sf_dummy_arg_name));

      make (u, sf_dummy_arg_name) (var ident);

      make (u, pointer_assignment_stmt) R.(var lbl_def * var name * var arrow * var target * var eos + var lbl_def * var name * var perc * var name * var arrow * var target * var eos + var lbl_def * var name * var perc * var data_ref * var arrow * var target * var eos + var lbl_def * var name * var lparen * var sf_expr_list * var rparen * var perc * var name * var arrow * var target * var eos + var lbl_def * var name * var lparen * var sf_expr_list * var rparen * var perc * var data_ref * var arrow * var target * var eos + var lbl_def * var name * var lparen * var sf_dummy_arg_name_list * var rparen * var perc * var name * var arrow * var target * var eos + var lbl_def * var name * var lparen * var sf_dummy_arg_name_list * var rparen * var perc * var data_ref * var arrow * var target * var eos);

      make (u, target) (var expr);

      make (u, where_stmt) R.(var lbl_def * var where_kw * var lparen * var mask_expr * var rparen * var assignment_stmt);

      make (u, mask_expr) (var expr);

      make (u, assignment_stmt) R.(var lbl_def * var name * var perc * var name * var assign * var expr * var eos + var lbl_def * var name * var perc * var data_ref * var assign * var expr * var eos + var lbl_def * var name * var lparen * var sf_expr_list * var rparen * var perc * var name * var assign * var expr * var eos + var lbl_def * var name * var lparen * var sf_expr_list * var rparen * var perc * var data_ref * var assign * var expr * var eos + var lbl_def * var name * var lparen * var sf_dummy_arg_name_list * var rparen * var perc * var name * var assign * var expr * var eos + var lbl_def * var name * var lparen * var sf_dummy_arg_name_list * var rparen * var perc * var data_ref * var assign * var expr * var eos + var lbl_def * var name * var assign * var expr * var eos + var lbl_def * var name * var lparen * var sf_expr_list * var rparen * var assign * var expr * var eos + var lbl_def * var name * var lparen * var sf_expr_list * var rparen * var substring_range * var assign * var expr * var eos);

      make (u, arithmetic_if_stmt) R.(var lbl_def * var if_kw * var lparen * var scalar_numeric_expr * var rparen * var lbl_ref * var comma * var lbl_ref * var comma * var lbl_ref * var eos);

      make (u, scalar_numeric_expr) (var expr);

      make (u, lbl_ref) (var int_literal_constant);

      make (u, assign_stmt) R.(var lbl_def * var assign_kw * var lbl_ref * var to_kw * var variable_name * var eos);

      make (u, backspace_stmt) R.(var lbl_def * var backspace_kw * var unit_identifier * var eos + var lbl_def * var backspace_kw * var lparen * var position_spec * star (var comma * var position_spec) * var rparen * var eos);

      make (u, unit_identifier) R.(var uf_expr + var ast);

      make (u, uf_expr) R.(var uf_term + var sign * var uf_term + var uf_expr * var add_op * var uf_term);

      make (u, uf_term) R.(var uf_factor + var uf_term * var mult_op * var uf_factor + var uf_term * var concat_op * var uf_primary);

      make (u, uf_factor) R.(var uf_primary + var uf_primary * var power_op * var uf_factor);

      make (u, uf_primary) R.(var int_literal_constant + var char_literal_constant + var name + var function_reference + var data_ref + var lparen * var uf_expr * var rparen);

      make (u, position_spec) R.(opt (var unit_eq_kw) * var unit_identifier + var iostat_eq_kw * var scalar_variable + var err_eq_kw * var lbl_ref);

      make (u, scalar_variable) R.(var variable_name + var array_element);

      make (u, call_stmt) R.(var lbl_def * var call_kw * var subroutine_name_use * var eos + var lbl_def * var call_kw * var subroutine_name_use * var lparen * opt (var actual_arg * star (var comma * var actual_arg)) * var rparen * var eos);

      make (u, subroutine_name_use) (var ident);

      make (u, actual_arg) R.(opt (var name * var assign) * var expr + opt (var name * var assign) * var ast * var lbl_ref);

      make (u, close_stmt) R.(var lbl_def * var close_kw * var lparen * var close_spec_list * var rparen * var eos);

      make (u, close_spec_list) R.(var close_spec * star (var comma * var close_spec));

      make (u, close_spec) R.(var unit_identifier + var unit_kw * var assign * var unit_identifier + var iostat_kw * var assign * var scalar_variable + var err_kw * var assign * var lbl_ref + var status_kw * var assign * var char_expr);

      make (u, char_expr) R.(opt (var char_expr * var concat_op) * var char_primary);

      make (u, char_primary) R.(var char_operand + var lparen * var char_expr * var rparen);

      make (u, char_operand) R.(var char_literal_constant + var name + var data_ref + var function_reference);

      make (u, continue_stmt) R.(var lbl_def * var continue_kw * var eos);

      make (u, endfile_stmt) R.(var lbl_def * var end_kw * var file_kw * var unit_identifier * var eos + var lbl_def * var end_kw * var file_kw * var lparen * var position_spec * star (var comma * var position_spec) * var rparen * var eos);

      make (u, goto_stmt) R.(var lbl_def * var go_to_kw * var lbl_ref * var eos);

      make (u, go_to_kw) R.(var go_kw * var to_kw);

      make (u, computed_goto_stmt) R.(var lbl_def * var go_to_kw * var lparen * var lbl_ref * star (var comma * var lbl_ref) * var rparen * opt (var comma) * var scalar_int_expr * var eos);

      make (u, scalar_int_expr) (var expr);

      make (u, assigned_goto_stmt) R.(var lbl_def * var go_to_kw * var variable_name * var eos + var lbl_def * var go_to_kw * var variable_name * opt (var comma) * var lparen * var lbl_ref * star (var comma * var lbl_ref) * var rparen * var eos);

      make (u, if_stmt) R.(var lbl_def * var if_kw * var lparen * var scalar_logical_expr * var rparen * var action_stmt);

      make (u, scalar_logical_expr) (var expr);

      make (u, inquire_stmt) R.(var lbl_def * var inquire_kw * var lparen * var inquire_spec_list * var rparen * var eos + var lbl_def * var inquire_kw * var lparen * var iolength_kw * var assign * var scalar_variable * var rparen * var output_item_list * var eos);

      make (u, inquire_spec_list) R.(var unit_identifier * var comma * var inquire_spec * star (var comma * var inquire_spec) + var inquire_spec * star (var comma * var inquire_spec));

      make (u, inquire_spec) R.(var unit_kw * var assign * var unit_identifier + var file_kw * var assign * var char_expr + var iostat_kw * var assign * var scalar_variable + var err_kw * var assign * var lbl_ref + var exist_kw * var assign * var scalar_variable + var opened_kw * var assign * var scalar_variable + var number_kw * var assign * var scalar_variable + var named_kw * var assign * var scalar_variable + var name_kw * var assign * var scalar_variable + var access_kw * var assign * var scalar_variable + var sequential_kw * var assign * var scalar_variable + var direct_kw * var assign * var scalar_variable + var form_kw * var assign * var scalar_variable + var formatted_kw * var assign * var scalar_variable + var unformatted_kw * var assign * var scalar_variable + var recl_kw * var assign * var expr + var nextrec_kw * var assign * var scalar_variable + var blank_kw * var assign * var scalar_variable + var position_kw * var assign * var scalar_variable + var action_kw * var assign * var scalar_variable + var read_kw * var assign * var scalar_variable + var write_kw * var assign * var scalar_variable + var readwrite_kw * var assign * var scalar_variable + var delim_kw * var assign * var scalar_variable + var pad_kw * var assign * var scalar_variable);

      make (u, output_item_list) R.(var output_item * star (var comma * var output_item));

      make (u, output_item) R.(var expr + var output_implied_do);

      make (u, output_implied_do) R.(var lparen * var output_item_list * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var rparen + var lparen * var output_item_list * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var comma * var expr * var rparen);

      make (u, open_stmt) R.(var lbl_def * var open_kw * var lparen * var connect_spec_list * var rparen * var eos);

      make (u, connect_spec_list) R.(var connect_spec * star (var comma * var connect_spec));

      make (u, connect_spec) R.(var unit_identifier + var unit_kw * var assign * var unit_identifier + var iostat_kw * var assign * var scalar_variable + var err_kw * var assign * var lbl_ref + var file_kw * var assign * var char_expr + var status_kw * var assign * var char_expr + var access_kw * var assign * var char_expr + var form_kw * var assign * var char_expr + var recl_kw * var assign * var expr + var blank_kw * var assign * var char_expr + var position_kw * var assign * var char_expr + var action_kw * var assign * var char_expr + var delim_kw * var assign * var char_expr + var pad_kw * var assign * var char_expr);

      make (u, pause_stmt) R.(var lbl_def * var pause_kw * opt (var int_literal_constant + var char_literal_constant) * var eos);

      make (u, print_stmt) R.(var lbl_def * var print_kw * var format_identifier * opt (var comma * var output_item_list) * var eos);

      make (u, format_identifier) R.(var lbl_ref + var char_expr + var ast);

      make (u, read_stmt) R.(var lbl_def * var read_kw * var rd_ctl_spec * opt (var input_item_list) * var eos + var lbl_def * var read_kw * var rd_fmt_id * var eos + var lbl_def * var read_kw * var rd_fmt_id * var comma * var input_item_list * var eos);

      make (u, rd_ctl_spec) R.(var rd_unit_id + var lparen * var rd_io_ctl_spec_list * var rparen);

      make (u, rd_unit_id) R.(var lparen * var uf_expr * var rparen + var lparen * var ast * var rparen);

      make (u, rd_io_ctl_spec_list) R.(var unit_identifier * var comma * var io_control_spec + var unit_identifier * var comma * var format_identifier + var io_control_spec + var rd_io_ctl_spec_list * var comma * var io_control_spec);

      make (u, io_control_spec) R.(var unit_kw * var assign * var unit_identifier + var fmt_kw * var assign * var format_identifier + var nml_kw * var assign * var namelist_group_name + var rec_kw * var assign * var expr + var iostat_kw * var assign * var scalar_variable + var err_kw * var assign * var lbl_ref + var end_kw * var assign * var lbl_ref + var advance_kw * var assign * var char_expr + var size_kw * var assign * var variable_ + var eor_kw * var assign * var lbl_ref);

      make (u, input_item_list) R.(var input_item * star (var comma * var input_item));

      make (u, input_item) R.(var name + var data_ref + var input_implied_do);

      make (u, input_implied_do) R.(var lparen * var input_item_list * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var rparen + var lparen * var input_item_list * var comma * var implied_do_variable * var assign * var expr * var comma * var expr * var comma * var expr * var rparen);

      make (u, rd_fmt_id) R.(var lbl_ref + var ast + var char_operand + var char_operand * var concat_op * var char_primary + var rd_fmt_id_expr * var concat_op * var char_primary);

      make (u, rd_fmt_id_expr) R.(var lparen * var uf_expr * var rparen);

      make (u, return_stmt) R.(var lbl_def * var return_kw * opt (var expr) * var eos);

      make (u, rewind_stmt) R.(var lbl_def * var rewind_kw * var unit_identifier * var eos + var lbl_def * var rewind_kw * var lparen * var position_spec * star (var comma * var position_spec) * var rparen * var eos);

      make (u, stop_stmt) R.(var lbl_def * var stop_kw * opt (var int_literal_constant + var char_literal_constant) * var eos);

      make (u, write_stmt) R.(var lbl_def * var write_kw * var lparen * var io_control_spec_list * var rparen * opt (var output_item_list) * var eos);

      make (u, io_control_spec_list) R.(var unit_identifier * var comma * opt (var format_identifier) + var unit_identifier * var comma * var io_control_spec + var io_control_spec + var io_control_spec_list * var comma * var io_control_spec);

      make (u, do_construct) R.(var block_do_construct + var label_do_stmt);

      make (u, block_do_construct) R.(var lbl_def * var do_kw * var lbl_ref * var eos + var lbl_def * var do_kw * var loop_control * var eos + var lbl_def * var do_kw * var eos + var lbl_def * var name * var colon * var do_kw * var lbl_ref * var loop_control * var eos + var lbl_def * var name * var colon * var do_kw * var lbl_ref * var eos + var lbl_def * var name * var colon * var do_kw * var loop_control * var eos + var lbl_def * var name * var colon * var do_kw * var eos);

      make (u, loop_control) R.(var while_kw * var lparen * var expr * var rparen + var variable_name * var assign * var expr * var comma * var expr * opt (var comma * var expr));

      make (u, label_do_stmt) R.(var lbl_def * var do_kw * var lbl_ref * opt (var comma) * var loop_control * var eos);

      make (u, if_construct) R.(var if_then_stmt * star (var execution_part_construct) * star (var else_if_stmt * star (var execution_part_construct)) * opt (var else_stmt * star (var execution_part_construct)) * var end_if_stmt);

      make (u, if_then_stmt) R.(var lbl_def * opt (var if_construct_name * var colon) * var if_kw * var lparen * var scalar_logical_expr * var rparen * var then_kw * var eos);

      make (u, if_construct_name) (var ident);

      make (u, execution_part_construct) R.(var executable_construct + var format_stmt + var data_stmt + var entry_stmt);

      make (u, else_if_stmt) R.(var lbl_def * var else_kw * var if_kw * var lparen * var scalar_logical_expr * var rparen * var then_kw * opt (var if_construct_name) * var eos);

      make (u, else_stmt) R.(var lbl_def * var else_kw * opt (var if_construct_name) * var eos);

      make (u, end_if_stmt) R.(var lbl_def * var end_kw * var if_kw * opt (var if_construct_name) * var eos);

      make (u, case_construct) R.(var lbl_def * var name * var colon * var select_kw * var case_kw * var lparen * var expr * var rparen * var eos * var select_case_range + var lbl_def * var select_kw * var case_kw * var lparen * var expr * var rparen * var eos * var select_case_range);

      make (u, select_case_range) R.(var select_case_body * var end_select_stmt + var end_select_stmt);

      make (u, select_case_body) R.(plus (var case_body_construct));

      make (u, case_body_construct) R.(var case_stmt + var execution_part_construct);

      make (u, case_stmt) R.(var lbl_def * var case_kw * var case_selector * opt (var name) * var eos);

      make (u, case_selector) R.(var lparen * var case_value_range * star (var comma * var case_value_range) * var rparen + var default_kw);

      make (u, case_value_range) R.(var expr + var expr * var colon + var colon * var expr + var expr * var colon * var expr);

      make (u, end_select_stmt) R.(var lbl_def * var end_kw * var select_kw * opt (var end_name) * var eos);

      make (u, where_construct) R.(var where_construct_stmt * star (var assignment_stmt) * opt (var elsewhere_stmt * star (var assignment_stmt)) * var end_where_stmt);

      make (u, where_construct_stmt) R.(var lbl_def * var where_kw * var lparen * var mask_expr * var rparen * var eos);

      make (u, elsewhere_stmt) R.(var lbl_def * var elsewhere_kw * var eos);

      make (u, end_where_stmt) R.(var lbl_def * var end_kw * var where_kw * var eos);

      make (u, end_do_stmt) R.(var lbl_def * var end_kw * var do_kw * opt (var name) * var eos);

      make (u, end_program_stmt) R.(var lbl_def * var end_kw * var eos + var lbl_def * var end_kw * var program_kw * opt (var end_name) * var eos);

      make (u, internal_sub_prog_part) R.(var body * var contains_stmt * var internal_subprogram + var contains_stmt * var internal_subprogram + var internal_sub_prog_part * var internal_subprogram);

      make (u, body) R.(plus (var body_construct));

      make (u, contains_stmt) R.(var lbl_def * var contains_kw * var eos);

      make (u, internal_subprogram) R.(var function_subprogram + var subroutine_subprogram);

      make (u, function_subprogram) R.(var lbl_def * var function_prefix * var function_name * var function_range);

      make (u, function_range) R.(var function_par_list * var eos * opt (var body) * var end_function_stmt + var function_par_list * var result_kw * var lparen * var name * var rparen * var eos * var internal_sub_prog_part * var end_function_stmt + var function_par_list * var result_kw * var lparen * var name * var rparen * var eos * var body * var end_function_stmt + var function_par_list * var result_kw * var lparen * var name * var rparen * var eos * var end_function_stmt + var function_par_list * var eos * var internal_sub_prog_part * var end_function_stmt);

      make (u, subroutine_subprogram) R.(var lbl_def * opt (var recursive_kw) * var subroutine_kw * var subroutine_name * var subroutine_range);

      make (u, subroutine_range) R.(opt (var subroutine_par_list) * var eos * opt (var body) * var end_subroutine_stmt + var subroutine_par_list * var eos * var internal_sub_prog_part * var end_subroutine_stmt);

      make (u, module_) R.(var module_stmt * var module_body * var end_module_stmt + var module_stmt * var end_module_stmt);

      make (u, module_stmt) R.(var lbl_def * var module_kw * var module_name * var eos);

      make (u, module_name) (var ident);

      make (u, module_body) R.(var specification_part_construct + var module_subprogram_part_construct + var module_body * var specification_part_construct + var module_body * var module_subprogram_part_construct);

      make (u, module_subprogram_part_construct) R.(var contains_stmt + var module_subprogram);

      make (u, module_subprogram) R.(var function_subprogram + var subroutine_subprogram);

      make (u, end_module_stmt) R.(var lbl_def * var end_kw * var eos + var lbl_def * var end_kw * var module_kw * opt (var end_name) * var eos);

      make (u, block_data_subprogram) R.(var block_data_stmt * var block_data_body * var end_block_data_stmt + var block_data_stmt * var end_block_data_stmt);

      make (u, block_data_stmt) R.(var lbl_def * var block_kw * var data_kw * opt (var block_data_name) * var eos);

      make (u, block_data_name) (var ident);

      make (u, block_data_body) R.(plus (var block_data_body_construct));

      make (u, block_data_body_construct) (var specification_part_construct);

      make (u, end_block_data_stmt) R.(var lbl_def * var end_kw * var block_kw * var data_kw * opt (var end_name) * var eos + var lbl_def * var end_kw * var eos);

      make (u, eos) R.(plus (var newline));
    ]

  let scanner =
    let letter_ = R.(range "A" "Z" + range "a" "z") in
    let digit = range "0" "9" in
    let underscore = codes "_" in
    let alphanumeric_character = R.(letter_ + digit + underscore) in
    let sign_ = codes "+-" in
    let digit_string = R.(plus digit) in
    let signed_digit_string = R.(sign_ * digit_string) in
    let ident_ = R.(letter_ * star alphanumeric_character) in
    let kind_param = R.(digit_string + ident_) in
    let int_literal_constant_ = R.(digit_string * opt (codes "_" * kind_param)) in
    let char_literal_constant_ = R.(opt (kind_param * codes "_") * codes "'" * star (not_codes "'") * codes "'" +
                                    opt (kind_param * codes "_") * codes "\"" * star (not_codes "\"") * codes "\"") in
    let significand = R.(digit_string * codes "." * opt digit_string + codes "." * digit_string) in
    let exponent_letter = codes "Ee" in
    let exponent = R.(signed_digit_string + digit_string) in
    let hex_digit = R.(digit + range "A" "F" + range "a" "f") in
    let octal_digit = range "0" "7" in
    let binary_digit = codes "01" in
    Production.[
      make (u, ident) ident_;
      make (u, sign) sign_;
      make (u, letter) letter_;
      make (u, int_literal_constant) int_literal_constant_;
      make (u, scalar_int_literal_constant) digit_string;
      make (u, real_literal_constant) R.(significand * opt (exponent_letter * exponent) * opt (codes "_" * kind_param) +
                                         digit_string * exponent_letter * exponent * opt (codes "_" * kind_param));
      make (u, char_literal_constant) char_literal_constant_;
      make (u, hex_constant) R.(codes "Zz" * codes "'" * plus hex_digit * codes "'" +
                                codes "Zz" * codes "\"" * plus hex_digit * codes "\"");
      make (u, octal_constant) R.(codes "Oo" * codes "'" * plus octal_digit * codes "'" +
                                  codes "Oo" * codes "\"" * plus octal_digit * codes "\"");
      make (u, binary_constant) R.(codes "Bb" * codes "'" * plus binary_digit * codes "'" +
                                   codes "Bb" * codes "\"" * plus binary_digit * codes "\"");

      make (u, logical_constant) R.(text ".TRUE." * opt (codes "_" * kind_param) + text ".FALSE." * opt (codes "_" * kind_param));

      make (u, char_string_edit_descr) R.(char_literal_constant_ * digit_string * codes "Hh" * plus any);
      make (u, dop) R.(codes "." * plus letter_ * codes ".");
      make (u, label) int_literal_constant_;

      make (u, perc) (text "%");
      make (u, lparen) (text "(");
      make (u, larray) (text "(/");
      make (u, rparen) (text ")");
      make (u, ast) (text "*");
      make (u, astast) (text "**");
      make (u, plus_) (text "+");
      make (u, comma) (text ",");
      make (u, minus) (text "-");
      make (u, dot) (text ".");
      make (u, slash) (text "/");
      make (u, rarray) (text "/)");
      make (u, slashslash) (text "//");
      make (u, slasheq) (text "/=");
      make (u, colon) (text ":");
      make (u, coloncolon) (text "::");
      make (u, lt) (text "<");
      make (u, le) (text "<=");
      make (u, assign) (text "=");
      make (u, eq) (text "==");
      make (u, arrow) (text "=>");
      make (u, gt) (text ">");
      make (u, ge) (text ">=");
      make (u, a_kw) (text "A");
      make (u, access_kw) (text "ACCESS");
      make (u, action_kw) (text "ACTION");
      make (u, advance_kw) (text "ADVANCE");
      make (u, allocatable_kw) (text "ALLOCATABLE");
      make (u, allocate_kw) (text "ALLOCATE");
      make (u, and_op_kw) (text ".AND.");
      make (u, assign_kw) (text "ASSIGN");
      make (u, assignment_kw) (text "ASSIGNMENT");
      make (u, b_kw) (text "B");
      make (u, backspace_kw) (text "BACKSPACE");
      make (u, blank_kw) (text "BLANK");
      make (u, block_kw) (text "BLOCK");
      make (u, bn_kw) (text "BN");
      make (u, bz_kw) (text "BZ");
      make (u, call_kw) (text "CALL");
      make (u, case_kw) (text "CASE");
      make (u, character_kw) (text "CHARACTER");
      make (u, close_kw) (text "CLOSE");
      make (u, common_kw) (text "COMMON");
      make (u, complex_kw) (text "COMPLEX");
      make (u, contains_kw) (text "CONTAINS");
      make (u, continue_kw) (text "CONTINUE");
      make (u, cycle_kw) (text "CYCLE");
      make (u, d_kw) (text "D");
      make (u, data_kw) (text "DATA");
      make (u, deallocate_kw) (text "DEALLOCATE");
      make (u, default_kw) (text "DEFAULT");
      make (u, delim_kw) (text "DELIM");
      make (u, dimension_kw) (text "DIMENSION");
      make (u, direct_kw) (text "DIRECT");
      make (u, do_kw) (text "DO");
      make (u, double_kw) (text "DOUBLE");
      make (u, e_kw) (text "E");
      make (u, else_kw) (text "ELSE");
      make (u, elsewhere_kw) (text "ELSEWHERE");
      make (u, en_kw) (text "EN");
      make (u, end_kw) (text "END");
      make (u, entry_kw) (text "ENTRY");
      make (u, eor_kw) (text "EOR");
      make (u, eq_op_kw) (text ".EQ.");
      make (u, equivalence_kw) (text "EQUIVALENCE");
      make (u, eqv_op_kw) (text ".EQV.");
      make (u, err_kw) (text "ERR");
      make (u, err_eq_kw) (text "ERR=");
      make (u, es_kw) (text "ES");
      make (u, exist_kw) (text "EXIST");
      make (u, exit_kw) (text "EXIT");
      make (u, external_kw) (text "EXTERNAL");
      make (u, f_kw) (text "F");
      make (u, file_kw) (text "FILE");
      make (u, fmt_kw) (text "FMT");
      make (u, form_kw) (text "FORM");
      make (u, format_kw) (text "FORMAT");
      make (u, formatted_kw) (text "FORMATTED");
      make (u, function_kw) (text "FUNCTION");
      make (u, g_kw) (text "G");
      make (u, ge_op_kw) (text ".GE.");
      make (u, go_kw) (text "GO");
      make (u, gt_op_kw) (text ".GT.");
      make (u, i_kw) (text "I");
      make (u, if_kw) (text "IF");
      make (u, implicit_kw) (text "IMPLICIT");
      make (u, in_kw) (text "IN");
      make (u, inquire_kw) (text "INQUIRE");
      make (u, integer_kw) (text "INTEGER");
      make (u, intent_kw) (text "INTENT");
      make (u, interface_kw) (text "INTERFACE");
      make (u, intrinsic_kw) (text "INTRINSIC");
      make (u, iolength_kw) (text "IOLENGTH");
      make (u, iostat_kw) (text "IOSTAT");
      make (u, iostat_eq_kw) (text "IOSTAT=");
      make (u, kind_kw) (text "KIND");
      make (u, l_kw) (text "L");
      make (u, le_op_kw) (text ".LE.");
      make (u, len_kw) (text "LEN");
      make (u, logical_kw) (text "LOGICAL");
      make (u, lt_op_kw) (text ".LT.");
      make (u, module_kw) (text "MODULE");
      make (u, name_kw) (text "NAME");
      make (u, named_kw) (text "NAMED");
      make (u, namelist_kw) (text "NAMELIST");
      make (u, ne_op_kw) (text ".NE.");
      make (u, neqv_op_kw) (text ".NEQV.");
      make (u, nextrec_kw) (text "NEXTREC");
      make (u, nml_kw) (text "NML");
      make (u, none_kw) (text "NONE");
      make (u, not_op_kw) (text ".NOT.");
      make (u, nullify_kw) (text "NULLIFY");
      make (u, number_kw) (text "NUMBER");
      make (u, o_kw) (text "O");
      make (u, only_kw) (text "ONLY");
      make (u, open_kw) (text "OPEN");
      make (u, opened_kw) (text "OPENED");
      make (u, operator_kw) (text "OPERATOR");
      make (u, optional_kw) (text "OPTIONAL");
      make (u, or_op_kw) (text ".OR.");
      make (u, out_kw) (text "OUT");
      make (u, p_kw) (text "P");
      make (u, pad_kw) (text "PAD");
      make (u, parameter_kw) (text "PARAMETER");
      make (u, pause_kw) (text "PAUSE");
      make (u, pointer_kw) (text "POINTER");
      make (u, position_kw) (text "POSITION");
      make (u, precision_kw) (text "PRECISION");
      make (u, print_kw) (text "PRINT");
      make (u, private_kw) (text "PRIVATE");
      make (u, procedure_kw) (text "PROCEDURE");
      make (u, program_kw) (text "PROGRAM");
      make (u, public_kw) (text "PUBLIC");
      make (u, read_kw) (text "READ");
      make (u, readwrite_kw) (text "READWRITE");
      make (u, real_kw) (text "REAL");
      make (u, rec_kw) (text "REC");
      make (u, recl_kw) (text "RECL");
      make (u, recursive_kw) (text "RECURSIVE");
      make (u, result_kw) (text "RESULT");
      make (u, return_kw) (text "RETURN");
      make (u, rewind_kw) (text "REWIND");
      make (u, s_kw) (text "S");
      make (u, save_kw) (text "SAVE");
      make (u, select_kw) (text "SELECT");
      make (u, sequence_kw) (text "SEQUENCE");
      make (u, sequential_kw) (text "SEQUENTIAL");
      make (u, size_kw) (text "SIZE");
      make (u, sp_kw) (text "SP");
      make (u, ss_kw) (text "SS");
      make (u, stat_kw) (text "STAT");
      make (u, status_kw) (text "STATUS");
      make (u, stop_kw) (text "STOP");
      make (u, subroutine_kw) (text "SUBROUTINE");
      make (u, t_kw) (text "T");
      make (u, target_kw) (text "TARGET");
      make (u, then_kw) (text "THEN");
      make (u, tl_kw) (text "TL");
      make (u, to_kw) (text "TO");
      make (u, tr_kw) (text "TR");
      make (u, type_kw) (text "TYPE");
      make (u, unformatted_kw) (text "UNFORMATTED");
      make (u, unit_kw) (text "UNIT");
      make (u, unit_eq_kw) (text "UNIT=");
      make (u, use_kw) (text "USE");
      make (u, where_kw) (text "WHERE");
      make (u, while_kw) (text "WHILE");
      make (u, write_kw) (text "WRITE");
      make (u, x_kw) (text "X");
      make (u, z_kw) (text "Z");

      make (u, ws) R.(star (codes " \t"));
      make (u, newline) (codes "\n");
    ]
end)

module B = Benchmark.Make(X)

let _ =
  (*let d = X.driver (X.tables ()) in
  X.Run.file (fun c -> d#read c) "seed.f90";
  Fmt.pr "@[%a@]" Trace.pp d#trace;*)
  (*Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);
  Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))*)

  B.benchmark ()

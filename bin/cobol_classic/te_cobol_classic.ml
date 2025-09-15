open! Te_bot
open Te_core
open Te_top
module T = Types

module X = Spec.Classic(functor(Context: Spec.CONTEXT) -> struct
    open Context

    let s = variable_supply
    let (start, s) = variable s "start"
    let (ws, s) = variable s "ws"

    let (mode, s) = variable s "mode"
    let (priority_number, s) = variable s "priority_number"
    let (user_defined_word, s) = variable s "user_defined_word"
    let (system_name, s) = variable s "system_name"
    let (alphabetic_user_defined_word, s) = variable s "alphabetic_user_defined_word"
    let (copy_directive_without_period, s) = variable s "copy_directive_without_period"
    let (statements, s) = variable s "statements"
    let (overflow_phrases, s) = variable s "overflow_phrases"
    let (invalid_key_phrases, s) = variable s "invalid_key_phrases"
    let (at_end_phrases, s) = variable s "at_end_phrases"
    let (exception_phrases, s) = variable s "exception_phrases"
    let (size_error_phrases, s) = variable s "size_error_phrases"
    let (examine_statement, s) = variable s "examine_statement"
    let (not_at_eop, s) = variable s "not_at_eop"
    let (at_eop, s) = variable s "at_eop"
    let (write_before_after, s) = variable s "write_before_after"
    let (not_at_end, s) = variable s "not_at_end"
    let (at_end, s) = variable s "at_end"
    let (inspect_replacing_phrase, s) = variable s "inspect_replacing_phrase"
    let (inspect_tallying_phrase, s) = variable s "inspect_tallying_phrase"
    let (before_after_phrase, s) = variable s "before_after_phrase"
    let (when_other_phrase, s) = variable s "when_other_phrase"
    let (when_phrase, s) = variable s "when_phrase"
    let (e_phrase, s) = variable s "e_phrase"
    let (invalid_key, s) = variable s "invalid_key"
    let (not_invalid_key, s) = variable s "not_invalid_key"
    let (not_on_exception, s) = variable s "not_on_exception"
    let (on_exception, s) = variable s "on_exception"
    let (not_on_overflow, s) = variable s "not_on_overflow"
    let (on_overflow, s) = variable s "on_overflow"
    let (not_on_size_error, s) = variable s "not_on_size_error"
    let (on_size_error, s) = variable s "on_size_error"
    let (use_directive, s) = variable s "use_directive"
    let (subtract_statement, s) = variable s "subtract_statement"
    let (set_statement, s) = variable s "set_statement"
    let (search_statement, s) = variable s "search_statement"
    let (perform_statement, s) = variable s "perform_statement"
    let (open_statement, s) = variable s "open_statement"
    let (multiply_statement, s) = variable s "multiply_statement"
    let (move_statement, s) = variable s "move_statement"
    let (inspect_statement, s) = variable s "inspect_statement"
    let (go_to_statement, s) = variable s "go_to_statement"
    let (divide_statement, s) = variable s "divide_statement"
    let (close_statement, s) = variable s "close_statement"
    let (add_statement, s) = variable s "add_statement"
    let (accept_statement, s) = variable s "accept_statement"
    let (call_using_phrase, s) = variable s "call_using_phrase"
    let (altered_go_to, s) = variable s "altered_go_to"
    let (occurs_clause, s) = variable s "occurs_clause"
    let (data_clauses, s) = variable s "data_clauses"
    let (data_description_entry, s) = variable s "data_description_entry"
    let (code_set_clause, s) = variable s "code_set_clause"
    let (recording_mode_clause, s) = variable s "recording_mode_clause"
    let (linage_area_clause, s) = variable s "linage_area_clause"
    let (linage_clause, s) = variable s "linage_clause"
    let (data_records_clause, s) = variable s "data_records_clause"
    let (value_of_clause, s) = variable s "value_of_clause"
    let (label_records_clause, s) = variable s "label_records_clause"
    let (record_varying_clause, s) = variable s "record_varying_clause"
    let (record_clause, s) = variable s "record_clause"
    let (block_contains_clause, s) = variable s "block_contains_clause"
    let (global_clause, s) = variable s "global_clause"
    let (external_clause, s) = variable s "external_clause"
    let (file_clauses, s) = variable s "file_clauses"
    let (file_description_entry, s) = variable s "file_description_entry"
    let (status_clause, s) = variable s "status_clause"
    let (relative_key, s) = variable s "relative_key"
    let (record_key, s) = variable s "record_key"
    let (password_clause, s) = variable s "password_clause"
    let (key_clause, s) = variable s "key_clause"
    let (access_mode_clause, s) = variable s "access_mode_clause"
    let (record_delimiter_clause, s) = variable s "record_delimiter_clause"
    let (padding_character_clause, s) = variable s "padding_character_clause"
    let (organisation_clause, s) = variable s "organisation_clause"
    let (reserve_clause, s) = variable s "reserve_clause"
    let (file_control_clauses, s) = variable s "file_control_clauses"
    let (assign_clause, s) = variable s "assign_clause"
    let (select_clause, s) = variable s "select_clause"
    let (file_control_entry, s) = variable s "file_control_entry"
    let (special_names_clauses, s) = variable s "special_names_clauses"
    let (currency_clause, s) = variable s "currency_clause"
    let (class_clause, s) = variable s "class_clause"
    let (symbolic_clause, s) = variable s "symbolic_clause"
    let (alphabet_clause, s) = variable s "alphabet_clause"
    let (environment_clause, s) = variable s "environment_clause"
    let (computer_paragraphs, s) = variable s "computer_paragraphs"
    let (sentence, s) = variable s "sentence"
    let (series_of_imperative_statements, s) = variable s "series_of_imperative_statements"
    let (procedure_division_content, s) = variable s "procedure_division_content"
    let (section, s) = variable s "section"
    let (using_phrase, s) = variable s "using_phrase"
    let (record_description_entry, s) = variable s "record_description_entry"
    let (data_division_content, s) = variable s "data_division_content"
    let (file_section, s) = variable s "file_section"
    let (working_storage, s) = variable s "working_storage"
    let (linkage_section, s) = variable s "linkage_section"
    let (environment_division_content, s) = variable s "environment_division_content"
    let (identification_division_content, s) = variable s "identification_division_content"
    let (copy_operand, s) = variable s "copy_operand"
    let (abbreviation_rest, s) = variable s "abbreviation_rest"
    let (object_, s) = variable s "object_"
    let (relational_operator, s) = variable s "relational_operator"
    let (operand, s) = variable s "operand"
    let (simple_condition, s) = variable s "simple_condition"
    let (combinable_condition, s) = variable s "combinable_condition"
    let (condition, s) = variable s "condition"
    let (basis, s) = variable s "basis"
    let (power, s) = variable s "power"
    let (times_div, s) = variable s "times_div"
    let (arithmetic_expression, s) = variable s "arithmetic_expression"
    let (special_register, s) = variable s "special_register"
    let (assignment_name, s) = variable s "assignment_name"
    let (environment_name, s) = variable s "environment_name"
    let (computer_name, s) = variable s "computer_name"
    let (section_name, s) = variable s "section_name"
    let (paragraph_name, s) = variable s "paragraph_name"
    let (text_name, s) = variable s "text_name"
    let (program_name, s) = variable s "program_name"
    let (library_name, s) = variable s "library_name"
    let (symbolic_character, s) = variable s "symbolic_character"
    let (record_name, s) = variable s "record_name"
    let (mnemonic_name, s) = variable s "mnemonic_name"
    let (index_name, s) = variable s "index_name"
    let (file_name, s) = variable s "file_name"
    let (data_name, s) = variable s "data_name"
    let (condition_name, s) = variable s "condition_name"
    let (class_name, s) = variable s "class_name"
    let (alphabet_name, s) = variable s "alphabet_name"
    let (length, s) = variable s "length"
    let (leftmost_character_position, s) = variable s "leftmost_character_position"
    let (condition_name_reference, s) = variable s "condition_name_reference"
    let (subscript, s) = variable s "subscript"
    let (qualified_data_name, s) = variable s "qualified_data_name"
    let (identifier, s) = variable s "identifier"
    let (procedure_name, s) = variable s "procedure_name"
    let (abbreviated_combined_relation_condition, s) = variable s "abbreviated_combined_relation_condition"
    let (accept_statement_format_i, s) = variable s "accept_statement_format_i"
    let (accept_statement_format_ii, s) = variable s "accept_statement_format_ii"
    let (add_statement_format_i, s) = variable s "add_statement_format_i"
    let (add_statement_format_ii, s) = variable s "add_statement_format_ii"
    let (add_statement_format_iii, s) = variable s "add_statement_format_iii"
    let (after_phrase, s) = variable s "after_phrase"
    let (alter_statement, s) = variable s "alter_statement"
    let (blank_when_zero_clause, s) = variable s "blank_when_zero_clause"
    let (call_statement_format_i, s) = variable s "call_statement_format_i"
    let (call_statement_format_ii, s) = variable s "call_statement_format_ii"
    let (cancel_statement, s) = variable s "cancel_statement"
    let (class_condition, s) = variable s "class_condition"
    let (close_statement_format_i, s) = variable s "close_statement_format_i"
    let (close_statement_format_ii, s) = variable s "close_statement_format_ii"
    let (combined_condition, s) = variable s "combined_condition"
    let (compute_statement, s) = variable s "compute_statement"
    let (condition_name_condition, s) = variable s "condition_name_condition"
    let (condition_name_in_data_division, s) = variable s "condition_name_in_data_division"
    let (condition_name_in_special_names_paragraph, s) = variable s "condition_name_in_special_names_paragraph"
    let (configuration_section, s) = variable s "configuration_section"
    let (continue_statement, s) = variable s "continue_statement"
    let (copy_directive, s) = variable s "copy_directive"
    let (data_description_entry_format_i, s) = variable s "data_description_entry_format_i"
    let (delete_statement, s) = variable s "delete_statement"
    let (display_statement, s) = variable s "display_statement"
    let (divide_statement_format_i, s) = variable s "divide_statement_format_i"
    let (divide_statement_format_ii, s) = variable s "divide_statement_format_ii"
    let (divide_statement_format_iii, s) = variable s "divide_statement_format_iii"
    let (divide_statement_format_iv, s) = variable s "divide_statement_format_iv"
    let (divide_statement_format_v, s) = variable s "divide_statement_format_v"
    let (entry_statement, s) = variable s "entry_statement"
    let (evaluate_statement, s) = variable s "evaluate_statement"
    let (exit_program_statement, s) = variable s "exit_program_statement"
    let (exit_statement, s) = variable s "exit_statement"
    let (file_control_paragraph, s) = variable s "file_control_paragraph"
    let (go_to_statement_format_i, s) = variable s "go_to_statement_format_i"
    let (go_to_statement_format_ii, s) = variable s "go_to_statement_format_ii"
    let (go_to_statement_format_iv, s) = variable s "go_to_statement_format_iv"
    let (goback_statement, s) = variable s "goback_statement"
    let (i_o_control_paragraph, s) = variable s "i_o_control_paragraph"
    let (identifier_format_i, s) = variable s "identifier_format_i"
    let (identifier_format_ii, s) = variable s "identifier_format_ii"
    let (idx_entry, s) = variable s "idx_entry"
    let (if_statement, s) = variable s "if_statement"
    let (initialize_statement, s) = variable s "initialize_statement"
    let (input_output_section, s) = variable s "input_output_section"
    let (inspect_statement_format_i, s) = variable s "inspect_statement_format_i"
    let (inspect_statement_format_ii, s) = variable s "inspect_statement_format_ii"
    let (inspect_statement_format_iii, s) = variable s "inspect_statement_format_iii"
    let (inspect_statement_format_iv, s) = variable s "inspect_statement_format_iv"
    let (justified_clause, s) = variable s "justified_clause"
    let (merge_statement, s) = variable s "merge_statement"
    let (move_statement_format_i, s) = variable s "move_statement_format_i"
    let (move_statement_format_ii, s) = variable s "move_statement_format_ii"
    let (multiply_statement_format_i, s) = variable s "multiply_statement_format_i"
    let (multiply_statement_format_ii, s) = variable s "multiply_statement_format_ii"
    let (negated_simple_condition, s) = variable s "negated_simple_condition"
    let (cobol_source_program, s) = variable s "cobol_source_program"
    let (nested_source_program, s) = variable s "nested_source_program"
    let (object_computer_paragraph, s) = variable s "object_computer_paragraph"
    let (occurs_clause_format_i, s) = variable s "occurs_clause_format_i"
    let (occurs_clause_format_ii, s) = variable s "occurs_clause_format_ii"
    let (ocp_entry, s) = variable s "ocp_entry"
    let (open_statement_format_i, s) = variable s "open_statement_format_i"
    let (open_statement_format_ii, s) = variable s "open_statement_format_ii"
    let (para, s) = variable s "para"
    let (perform_statement_format_i, s) = variable s "perform_statement_format_i"
    let (perform_statement_format_ii, s) = variable s "perform_statement_format_ii"
    let (perform_statement_format_iii, s) = variable s "perform_statement_format_iii"
    let (perform_statement_format_iv, s) = variable s "perform_statement_format_iv"
    let (picture_clause, s) = variable s "picture_clause"
    let (qsam_or_sam_i_o_control_entries, s) = variable s "qsam_or_sam_i_o_control_entries"
    let (read_statement_format_i, s) = variable s "read_statement_format_i"
    let (read_statement_format_ii, s) = variable s "read_statement_format_ii"
    let (redefines_clause, s) = variable s "redefines_clause"
    let (reference_to_procedure_division_name_format_i, s) = variable s "reference_to_procedure_division_name_format_i"
    let (reference_to_procedure_division_name_format_ii, s) = variable s "reference_to_procedure_division_name_format_ii"
    let (relation_condition, s) = variable s "relation_condition"
    let (release_statement, s) = variable s "release_statement"
    let (renames_clause, s) = variable s "renames_clause"
    let (return_statement, s) = variable s "return_statement"
    let (rewrite_statement, s) = variable s "rewrite_statement"
    let (search_statement_format_i, s) = variable s "search_statement_format_i"
    let (search_statement_format_ii, s) = variable s "search_statement_format_ii"
    let (sect, s) = variable s "sect"
    let (set_statement_format_i, s) = variable s "set_statement_format_i"
    let (set_statement_format_ii, s) = variable s "set_statement_format_ii"
    let (set_statement_format_iii, s) = variable s "set_statement_format_iii"
    let (set_statement_format_iv, s) = variable s "set_statement_format_iv"
    let (set_statement_format_v, s) = variable s "set_statement_format_v"
    let (sign_clause, s) = variable s "sign_clause"
    let (sign_condition, s) = variable s "sign_condition"
    let (snp_entry, s) = variable s "snp_entry"
    let (sort_merge_i_o_control_entries, s) = variable s "sort_merge_i_o_control_entries"
    let (sort_statement, s) = variable s "sort_statement"
    let (source_computer_paragraph, s) = variable s "source_computer_paragraph"
    let (special_names_paragraph, s) = variable s "special_names_paragraph"
    let (start_statement, s) = variable s "start_statement"
    let (stop_statement, s) = variable s "stop_statement"
    let (string_statement, s) = variable s "string_statement"
    let (subtract_statement_format_i, s) = variable s "subtract_statement_format_i"
    let (subtract_statement_format_ii, s) = variable s "subtract_statement_format_ii"
    let (subtract_statement_format_iii, s) = variable s "subtract_statement_format_iii"
    let (switch_status_condition, s) = variable s "switch_status_condition"
    let (synchronized_clause, s) = variable s "synchronized_clause"
    let (test_phrase, s) = variable s "test_phrase"
    let (unstring_statement, s) = variable s "unstring_statement"
    let (usage_clause, s) = variable s "usage_clause"
    let (use_directive_format_i, s) = variable s "use_directive_format_i"
    let (use_directive_format_ii, s) = variable s "use_directive_format_ii"
    let (use_directive_format_iii, s) = variable s "use_directive_format_iii"
    let (value_clause_format_ii, s) = variable s "value_clause_format_ii"
    let (varying_phrase, s) = variable s "varying_phrase"
    let (vsam_i_o_control_entries, s) = variable s "vsam_i_o_control_entries"
    let (w_phrase, s) = variable s "w_phrase"
    let (write_statement_format_i, s) = variable s "write_statement_format_i"
    let (data_description_entry_format_ii, s) = variable s "data_description_entry_format_ii"
    let (data_description_entry_format_iii, s) = variable s "data_description_entry_format_iii"
    let (literal, s) = variable s "literal"
    let (figurative_constant, s) = variable s "figurative_constant"
    let (usage_keyword, s) = variable s "usage_keyword"

    let (quoted_pseudo_text, s) = variable s "quoted_pseudo_text"
    let (id, s) = variable s "id"
    let (cobol_word, s) = variable s "cobol_word"
    let (nonnumeric, s) = variable s "nonnumeric"
    let (numeric, s) = variable s "numeric"
    let (integer, s) = variable s "integer"
    let (level_number, s) = variable s "level_number"
    let (sixsix, s) = variable s "sixsix"
    let (eighteight, s) = variable s "eighteight"
    let (picture_string, s) = variable s "picture_string"
    let (comment_entry, s) = variable s "comment_entry"
    let (lparen, s) = variable s "lparen"
    let (rparen, s) = variable s "rparen"
    let (ast, s) = variable s "ast"
    let (astast, s) = variable s "astast"
    let (plus_, s) = variable s "plus_"
    let (minus, s) = variable s "minus"
    let (dot, s) = variable s "dot"
    let (slash, s) = variable s "slash"
    let (colon, s) = variable s "colon"
    let (lt, s) = variable s "lt"
    let (le, s) = variable s "le"
    let (eq, s) = variable s "eq"
    let (gt, s) = variable s "gt"
    let (ge, s) = variable s "ge"
    let (accept_kw, s) = variable s "accept_kw"
    let (access_kw, s) = variable s "access_kw"
    let (add_kw, s) = variable s "add_kw"
    let (address_kw, s) = variable s "address_kw"
    let (advancing_kw, s) = variable s "advancing_kw"
    let (after_kw, s) = variable s "after_kw"
    let (all_kw, s) = variable s "all_kw"
    let (alphabet_kw, s) = variable s "alphabet_kw"
    let (alphabetic_kw, s) = variable s "alphabetic_kw"
    let (alphabetic_lower_kw, s) = variable s "alphabetic_lower_kw"
    let (alphabetic_upper_kw, s) = variable s "alphabetic_upper_kw"
    let (alphanumeric_kw, s) = variable s "alphanumeric_kw"
    let (alphanumeric_edited_kw, s) = variable s "alphanumeric_edited_kw"
    let (also_kw, s) = variable s "also_kw"
    let (alter_kw, s) = variable s "alter_kw"
    let (alternate_kw, s) = variable s "alternate_kw"
    let (and_kw, s) = variable s "and_kw"
    let (any_kw, s) = variable s "any_kw"
    let (apply_kw, s) = variable s "apply_kw"
    let (are_kw, s) = variable s "are_kw"
    let (area_kw, s) = variable s "area_kw"
    let (areas_kw, s) = variable s "areas_kw"
    let (ascending_kw, s) = variable s "ascending_kw"
    let (assign_kw, s) = variable s "assign_kw"
    let (at_kw, s) = variable s "at_kw"
    let (author_kw, s) = variable s "author_kw"
    let (before_kw, s) = variable s "before_kw"
    let (beginning_kw, s) = variable s "beginning_kw"
    let (binary_kw, s) = variable s "binary_kw"
    let (blank_kw, s) = variable s "blank_kw"
    let (block_kw, s) = variable s "block_kw"
    let (bottom_kw, s) = variable s "bottom_kw"
    let (by_kw, s) = variable s "by_kw"
    let (call_kw, s) = variable s "call_kw"
    let (cancel_kw, s) = variable s "cancel_kw"
    let (character_kw, s) = variable s "character_kw"
    let (characters_kw, s) = variable s "characters_kw"
    let (class_kw, s) = variable s "class_kw"
    let (close_kw, s) = variable s "close_kw"
    let (code_set_kw, s) = variable s "code_set_kw"
    let (collating_kw, s) = variable s "collating_kw"
    let (comma_kw, s) = variable s "comma_kw"
    let (common_kw, s) = variable s "common_kw"
    let (comp_kw, s) = variable s "comp_kw"
    let (comp_1_kw, s) = variable s "comp_1_kw"
    let (comp_2_kw, s) = variable s "comp_2_kw"
    let (comp_3_kw, s) = variable s "comp_3_kw"
    let (comp_4_kw, s) = variable s "comp_4_kw"
    let (comp_5_kw, s) = variable s "comp_5_kw"
    let (computational_kw, s) = variable s "computational_kw"
    let (computational_1_kw, s) = variable s "computational_1_kw"
    let (computational_2_kw, s) = variable s "computational_2_kw"
    let (computational_3_kw, s) = variable s "computational_3_kw"
    let (computational_4_kw, s) = variable s "computational_4_kw"
    let (computational_5_kw, s) = variable s "computational_5_kw"
    let (compute_kw, s) = variable s "compute_kw"
    let (configuration_kw, s) = variable s "configuration_kw"
    let (contains_kw, s) = variable s "contains_kw"
    let (content_kw, s) = variable s "content_kw"
    let (continue_kw, s) = variable s "continue_kw"
    let (converting_kw, s) = variable s "converting_kw"
    let (copy_kw, s) = variable s "copy_kw"
    let (corr_kw, s) = variable s "corr_kw"
    let (corresponding_kw, s) = variable s "corresponding_kw"
    let (count_kw, s) = variable s "count_kw"
    let (currency_kw, s) = variable s "currency_kw"
    let (data_kw, s) = variable s "data_kw"
    let (date_kw, s) = variable s "date_kw"
    let (date_compiled_kw, s) = variable s "date_compiled_kw"
    let (date_written_kw, s) = variable s "date_written_kw"
    let (day_kw, s) = variable s "day_kw"
    let (day_of_week_kw, s) = variable s "day_of_week_kw"
    let (dbcs_kw, s) = variable s "dbcs_kw"
    let (debugging_kw, s) = variable s "debugging_kw"
    let (debug_item_kw, s) = variable s "debug_item_kw"
    let (decimal_point_kw, s) = variable s "decimal_point_kw"
    let (declaratives_kw, s) = variable s "declaratives_kw"
    let (delete_kw, s) = variable s "delete_kw"
    let (delimited_kw, s) = variable s "delimited_kw"
    let (delimiter_kw, s) = variable s "delimiter_kw"
    let (depending_kw, s) = variable s "depending_kw"
    let (descending_kw, s) = variable s "descending_kw"
    let (display_kw, s) = variable s "display_kw"
    let (display_1_kw, s) = variable s "display_1_kw"
    let (divide_kw, s) = variable s "divide_kw"
    let (division_kw, s) = variable s "division_kw"
    let (down_kw, s) = variable s "down_kw"
    let (duplicates_kw, s) = variable s "duplicates_kw"
    let (dynamic_kw, s) = variable s "dynamic_kw"
    let (ebcdic_kw, s) = variable s "ebcdic_kw"
    let (egcs_kw, s) = variable s "egcs_kw"
    let (else_kw, s) = variable s "else_kw"
    let (end_kw, s) = variable s "end_kw"
    let (end_add_kw, s) = variable s "end_add_kw"
    let (end_call_kw, s) = variable s "end_call_kw"
    let (end_compute_kw, s) = variable s "end_compute_kw"
    let (end_delete_kw, s) = variable s "end_delete_kw"
    let (end_divide_kw, s) = variable s "end_divide_kw"
    let (end_evaluate_kw, s) = variable s "end_evaluate_kw"
    let (end_if_kw, s) = variable s "end_if_kw"
    let (ending_kw, s) = variable s "ending_kw"
    let (end_multiply_kw, s) = variable s "end_multiply_kw"
    let (end_of_page_kw, s) = variable s "end_of_page_kw"
    let (end_perform_kw, s) = variable s "end_perform_kw"
    let (end_read_kw, s) = variable s "end_read_kw"
    let (end_return_kw, s) = variable s "end_return_kw"
    let (end_rewrite_kw, s) = variable s "end_rewrite_kw"
    let (end_search_kw, s) = variable s "end_search_kw"
    let (end_start_kw, s) = variable s "end_start_kw"
    let (end_string_kw, s) = variable s "end_string_kw"
    let (end_subtract_kw, s) = variable s "end_subtract_kw"
    let (end_unstring_kw, s) = variable s "end_unstring_kw"
    let (end_write_kw, s) = variable s "end_write_kw"
    let (entry_kw, s) = variable s "entry_kw"
    let (environment_kw, s) = variable s "environment_kw"
    let (eop_kw, s) = variable s "eop_kw"
    let (equal_kw, s) = variable s "equal_kw"
    let (error_kw, s) = variable s "error_kw"
    let (evaluate_kw, s) = variable s "evaluate_kw"
    let (every_kw, s) = variable s "every_kw"
    let (examine_kw, s) = variable s "examine_kw"
    let (exception_kw, s) = variable s "exception_kw"
    let (exit_kw, s) = variable s "exit_kw"
    let (extend_kw, s) = variable s "extend_kw"
    let (external_kw, s) = variable s "external_kw"
    let (false_kw, s) = variable s "false_kw"
    let (fd_kw, s) = variable s "fd_kw"
    let (file_kw, s) = variable s "file_kw"
    let (file_control_kw, s) = variable s "file_control_kw"
    let (filler_kw, s) = variable s "filler_kw"
    let (first_kw, s) = variable s "first_kw"
    let (footing_kw, s) = variable s "footing_kw"
    let (for_kw, s) = variable s "for_kw"
    let (from_kw, s) = variable s "from_kw"
    let (giving_kw, s) = variable s "giving_kw"
    let (global_kw, s) = variable s "global_kw"
    let (go_kw, s) = variable s "go_kw"
    let (goback_kw, s) = variable s "goback_kw"
    let (greater_kw, s) = variable s "greater_kw"
    let (high_value_kw, s) = variable s "high_value_kw"
    let (high_values_kw, s) = variable s "high_values_kw"
    let (id_kw, s) = variable s "id_kw"
    let (identification_kw, s) = variable s "identification_kw"
    let (if_kw, s) = variable s "if_kw"
    let (in_kw, s) = variable s "in_kw"
    let (index_kw, s) = variable s "index_kw"
    let (indexed_kw, s) = variable s "indexed_kw"
    let (initial_kw, s) = variable s "initial_kw"
    let (initialize_kw, s) = variable s "initialize_kw"
    let (input_kw, s) = variable s "input_kw"
    let (input_output_kw, s) = variable s "input_output_kw"
    let (inspect_kw, s) = variable s "inspect_kw"
    let (installation_kw, s) = variable s "installation_kw"
    let (into_kw, s) = variable s "into_kw"
    let (invalid_kw, s) = variable s "invalid_kw"
    let (i_o_kw, s) = variable s "i_o_kw"
    let (i_o_control_kw, s) = variable s "i_o_control_kw"
    let (is_kw, s) = variable s "is_kw"
    let (just_kw, s) = variable s "just_kw"
    let (justified_kw, s) = variable s "justified_kw"
    let (kanji_kw, s) = variable s "kanji_kw"
    let (key_kw, s) = variable s "key_kw"
    let (label_kw, s) = variable s "label_kw"
    let (leading_kw, s) = variable s "leading_kw"
    let (left_kw, s) = variable s "left_kw"
    let (length_kw, s) = variable s "length_kw"
    let (less_kw, s) = variable s "less_kw"
    let (linage_kw, s) = variable s "linage_kw"
    let (linage_counter_kw, s) = variable s "linage_counter_kw"
    let (line_kw, s) = variable s "line_kw"
    let (lines_kw, s) = variable s "lines_kw"
    let (linkage_kw, s) = variable s "linkage_kw"
    let (lock_kw, s) = variable s "lock_kw"
    let (low_value_kw, s) = variable s "low_value_kw"
    let (low_values_kw, s) = variable s "low_values_kw"
    let (memory_kw, s) = variable s "memory_kw"
    let (merge_kw, s) = variable s "merge_kw"
    let (mode_kw, s) = variable s "mode_kw"
    let (modules_kw, s) = variable s "modules_kw"
    let (more_labels_kw, s) = variable s "more_labels_kw"
    let (move_kw, s) = variable s "move_kw"
    let (multiple_kw, s) = variable s "multiple_kw"
    let (multiply_kw, s) = variable s "multiply_kw"
    let (native_kw, s) = variable s "native_kw"
    let (negative_kw, s) = variable s "negative_kw"
    let (next_kw, s) = variable s "next_kw"
    let (no_kw, s) = variable s "no_kw"
    let (not_kw, s) = variable s "not_kw"
    let (null_kw, s) = variable s "null_kw"
    let (nulls_kw, s) = variable s "nulls_kw"
    let (numeric_kw, s) = variable s "numeric_kw"
    let (numeric_edited_kw, s) = variable s "numeric_edited_kw"
    let (object_computer_kw, s) = variable s "object_computer_kw"
    let (occurs_kw, s) = variable s "occurs_kw"
    let (of_kw, s) = variable s "of_kw"
    let (off_kw, s) = variable s "off_kw"
    let (omitted_kw, s) = variable s "omitted_kw"
    let (on_kw, s) = variable s "on_kw"
    let (open_kw, s) = variable s "open_kw"
    let (optional_kw, s) = variable s "optional_kw"
    let (or_kw, s) = variable s "or_kw"
    let (order_kw, s) = variable s "order_kw"
    let (organization_kw, s) = variable s "organization_kw"
    let (other_kw, s) = variable s "other_kw"
    let (output_kw, s) = variable s "output_kw"
    let (overflow_kw, s) = variable s "overflow_kw"
    let (packed_decimal_kw, s) = variable s "packed_decimal_kw"
    let (padding_kw, s) = variable s "padding_kw"
    let (page_kw, s) = variable s "page_kw"
    let (password_kw, s) = variable s "password_kw"
    let (perform_kw, s) = variable s "perform_kw"
    let (pic_kw, s) = variable s "pic_kw"
    let (picture_kw, s) = variable s "picture_kw"
    let (pointer_kw, s) = variable s "pointer_kw"
    let (position_kw, s) = variable s "position_kw"
    let (positive_kw, s) = variable s "positive_kw"
    let (procedure_kw, s) = variable s "procedure_kw"
    let (procedures_kw, s) = variable s "procedures_kw"
    let (proceed_kw, s) = variable s "proceed_kw"
    let (program_kw, s) = variable s "program_kw"
    let (program_id_kw, s) = variable s "program_id_kw"
    let (quote_kw, s) = variable s "quote_kw"
    let (quotes_kw, s) = variable s "quotes_kw"
    let (random_kw, s) = variable s "random_kw"
    let (read_kw, s) = variable s "read_kw"
    let (record_kw, s) = variable s "record_kw"
    let (recording_kw, s) = variable s "recording_kw"
    let (records_kw, s) = variable s "records_kw"
    let (redefines_kw, s) = variable s "redefines_kw"
    let (reel_kw, s) = variable s "reel_kw"
    let (reference_kw, s) = variable s "reference_kw"
    let (relative_kw, s) = variable s "relative_kw"
    let (release_kw, s) = variable s "release_kw"
    let (remainder_kw, s) = variable s "remainder_kw"
    let (remarks_kw, s) = variable s "remarks_kw"
    let (removal_kw, s) = variable s "removal_kw"
    let (renames_kw, s) = variable s "renames_kw"
    let (replacing_kw, s) = variable s "replacing_kw"
    let (rerun_kw, s) = variable s "rerun_kw"
    let (reserve_kw, s) = variable s "reserve_kw"
    let (return_kw, s) = variable s "return_kw"
    let (return_code_kw, s) = variable s "return_code_kw"
    let (reversed_kw, s) = variable s "reversed_kw"
    let (rewind_kw, s) = variable s "rewind_kw"
    let (rewrite_kw, s) = variable s "rewrite_kw"
    let (right_kw, s) = variable s "right_kw"
    let (rounded_kw, s) = variable s "rounded_kw"
    let (run_kw, s) = variable s "run_kw"
    let (same_kw, s) = variable s "same_kw"
    let (sd_kw, s) = variable s "sd_kw"
    let (search_kw, s) = variable s "search_kw"
    let (section_kw, s) = variable s "section_kw"
    let (security_kw, s) = variable s "security_kw"
    let (segment_limit_kw, s) = variable s "segment_limit_kw"
    let (select_kw, s) = variable s "select_kw"
    let (sentence_kw, s) = variable s "sentence_kw"
    let (separate_kw, s) = variable s "separate_kw"
    let (sequence_kw, s) = variable s "sequence_kw"
    let (sequential_kw, s) = variable s "sequential_kw"
    let (set_kw, s) = variable s "set_kw"
    let (shift_in_kw, s) = variable s "shift_in_kw"
    let (shift_out_kw, s) = variable s "shift_out_kw"
    let (sign_kw, s) = variable s "sign_kw"
    let (size_kw, s) = variable s "size_kw"
    let (sort_kw, s) = variable s "sort_kw"
    let (sort_control_kw, s) = variable s "sort_control_kw"
    let (sort_core_size_kw, s) = variable s "sort_core_size_kw"
    let (sort_file_size_kw, s) = variable s "sort_file_size_kw"
    let (sort_merge_kw, s) = variable s "sort_merge_kw"
    let (sort_message_kw, s) = variable s "sort_message_kw"
    let (sort_mode_size_kw, s) = variable s "sort_mode_size_kw"
    let (sort_return_kw, s) = variable s "sort_return_kw"
    let (source_computer_kw, s) = variable s "source_computer_kw"
    let (space_kw, s) = variable s "space_kw"
    let (spaces_kw, s) = variable s "spaces_kw"
    let (special_names_kw, s) = variable s "special_names_kw"
    let (standard_kw, s) = variable s "standard_kw"
    let (standard_1_kw, s) = variable s "standard_1_kw"
    let (standard_2_kw, s) = variable s "standard_2_kw"
    let (start_kw, s) = variable s "start_kw"
    let (status_kw, s) = variable s "status_kw"
    let (stop_kw, s) = variable s "stop_kw"
    let (string_kw, s) = variable s "string_kw"
    let (subtract_kw, s) = variable s "subtract_kw"
    let (suppress_kw, s) = variable s "suppress_kw"
    let (symbolic_kw, s) = variable s "symbolic_kw"
    let (sync_kw, s) = variable s "sync_kw"
    let (synchronized_kw, s) = variable s "synchronized_kw"
    let (tally_kw, s) = variable s "tally_kw"
    let (tallying_kw, s) = variable s "tallying_kw"
    let (tape_kw, s) = variable s "tape_kw"
    let (test_kw, s) = variable s "test_kw"
    let (than_kw, s) = variable s "than_kw"
    let (then_kw, s) = variable s "then_kw"
    let (through_kw, s) = variable s "through_kw"
    let (thru_kw, s) = variable s "thru_kw"
    let (time_kw, s) = variable s "time_kw"
    let (times_kw, s) = variable s "times_kw"
    let (to_kw, s) = variable s "to_kw"
    let (top_kw, s) = variable s "top_kw"
    let (trailing_kw, s) = variable s "trailing_kw"
    let (true_kw, s) = variable s "true_kw"
    let (unit_kw, s) = variable s "unit_kw"
    let (unstring_kw, s) = variable s "unstring_kw"
    let (until_kw, s) = variable s "until_kw"
    let (up_kw, s) = variable s "up_kw"
    let (upon_kw, s) = variable s "upon_kw"
    let (usage_kw, s) = variable s "usage_kw"
    let (use_kw, s) = variable s "use_kw"
    let (using_kw, s) = variable s "using_kw"
    let (value_kw, s) = variable s "value_kw"
    let (values_kw, s) = variable s "values_kw"
    let (varying_kw, s) = variable s "varying_kw"
    let (when_kw, s) = variable s "when_kw"
    let (when_compiled_kw, s) = variable s "when_compiled_kw"
    let (with_kw, s) = variable s "with_kw"
    let (words_kw, s) = variable s "words_kw"
    let (working_storage_kw, s) = variable s "working_storage_kw"
    let (write_kw, s) = variable s "write_kw"
    let (write_only_kw, s) = variable s "write_only_kw"
    let (zero_kw, s) = variable s "zero_kw"
    let (zeroes_kw, s) = variable s "zeroes_kw"
    let (zeros_kw, s) = variable s "zeros_kw"

    let (u, s) = variable s "u"
    let (e, s) = variable s "e"
    let (e', s) = variable s "e'"

    let syntactic = [
      e';
      start;
      mode;
      priority_number;
      user_defined_word;
      system_name;
      alphabetic_user_defined_word;
      copy_directive_without_period;
      statements;
      overflow_phrases;
      invalid_key_phrases;
      at_end_phrases;
      exception_phrases;
      size_error_phrases;
      examine_statement;
      not_at_eop;
      at_eop;
      write_before_after;
      not_at_end;
      at_end;
      inspect_replacing_phrase;
      inspect_tallying_phrase;
      before_after_phrase;
      when_other_phrase;
      when_phrase;
      e_phrase;
      invalid_key;
      not_invalid_key;
      not_on_exception;
      on_exception;
      not_on_overflow;
      on_overflow;
      not_on_size_error;
      on_size_error;
      use_directive;
      subtract_statement;
      set_statement;
      search_statement;
      perform_statement;
      open_statement;
      multiply_statement;
      move_statement;
      inspect_statement;
      go_to_statement;
      divide_statement;
      close_statement;
      add_statement;
      accept_statement;
      call_using_phrase;
      altered_go_to;
      occurs_clause;
      data_clauses;
      data_description_entry;
      code_set_clause;
      recording_mode_clause;
      linage_area_clause;
      linage_clause;
      data_records_clause;
      value_of_clause;
      label_records_clause;
      record_varying_clause;
      record_clause;
      block_contains_clause;
      global_clause;
      external_clause;
      file_clauses;
      file_description_entry;
      status_clause;
      relative_key;
      record_key;
      password_clause;
      key_clause;
      access_mode_clause;
      record_delimiter_clause;
      padding_character_clause;
      organisation_clause;
      reserve_clause;
      file_control_clauses;
      assign_clause;
      select_clause;
      file_control_entry;
      special_names_clauses;
      currency_clause;
      class_clause;
      symbolic_clause;
      alphabet_clause;
      environment_clause;
      computer_paragraphs;
      sentence;
      series_of_imperative_statements;
      procedure_division_content;
      section;
      using_phrase;
      record_description_entry;
      data_division_content;
      file_section;
      working_storage;
      linkage_section;
      environment_division_content;
      identification_division_content;
      copy_operand;
      abbreviation_rest;
      object_;
      relational_operator;
      operand;
      simple_condition;
      combinable_condition;
      condition;
      basis;
      power;
      times_div;
      arithmetic_expression;
      special_register;
      assignment_name;
      environment_name;
      computer_name;
      section_name;
      paragraph_name;
      text_name;
      program_name;
      library_name;
      symbolic_character;
      record_name;
      mnemonic_name;
      index_name;
      file_name;
      data_name;
      condition_name;
      class_name;
      alphabet_name;
      length;
      leftmost_character_position;
      condition_name_reference;
      subscript;
      qualified_data_name;
      identifier;
      procedure_name;
      abbreviated_combined_relation_condition;
      accept_statement_format_i;
      accept_statement_format_ii;
      add_statement_format_i;
      add_statement_format_ii;
      add_statement_format_iii;
      after_phrase;
      alter_statement;
      blank_when_zero_clause;
      call_statement_format_i;
      call_statement_format_ii;
      cancel_statement;
      class_condition;
      close_statement_format_i;
      close_statement_format_ii;
      combined_condition;
      compute_statement;
      condition_name_condition;
      condition_name_in_data_division;
      condition_name_in_special_names_paragraph;
      configuration_section;
      continue_statement;
      copy_directive;
      data_description_entry_format_i;
      delete_statement;
      display_statement;
      divide_statement_format_i;
      divide_statement_format_ii;
      divide_statement_format_iii;
      divide_statement_format_iv;
      divide_statement_format_v;
      entry_statement;
      evaluate_statement;
      exit_program_statement;
      exit_statement;
      file_control_paragraph;
      go_to_statement_format_i;
      go_to_statement_format_ii;
      go_to_statement_format_iv;
      goback_statement;
      i_o_control_paragraph;
      identifier_format_i;
      identifier_format_ii;
      idx_entry;
      if_statement;
      initialize_statement;
      input_output_section;
      inspect_statement_format_i;
      inspect_statement_format_ii;
      inspect_statement_format_iii;
      inspect_statement_format_iv;
      justified_clause;
      merge_statement;
      move_statement_format_i;
      move_statement_format_ii;
      multiply_statement_format_i;
      multiply_statement_format_ii;
      negated_simple_condition;
      cobol_source_program;
      nested_source_program;
      object_computer_paragraph;
      occurs_clause_format_i;
      occurs_clause_format_ii;
      ocp_entry;
      open_statement_format_i;
      open_statement_format_ii;
      para;
      perform_statement_format_i;
      perform_statement_format_ii;
      perform_statement_format_iii;
      perform_statement_format_iv;
      picture_clause;
      qsam_or_sam_i_o_control_entries;
      read_statement_format_i;
      read_statement_format_ii;
      redefines_clause;
      reference_to_procedure_division_name_format_i;
      reference_to_procedure_division_name_format_ii;
      relation_condition;
      release_statement;
      renames_clause;
      return_statement;
      rewrite_statement;
      search_statement_format_i;
      search_statement_format_ii;
      sect;
      set_statement_format_i;
      set_statement_format_ii;
      set_statement_format_iii;
      set_statement_format_iv;
      set_statement_format_v;
      sign_clause;
      sign_condition;
      snp_entry;
      sort_merge_i_o_control_entries;
      sort_statement;
      source_computer_paragraph;
      special_names_paragraph;
      start_statement;
      stop_statement;
      string_statement;
      subtract_statement_format_i;
      subtract_statement_format_ii;
      subtract_statement_format_iii;
      switch_status_condition;
      synchronized_clause;
      test_phrase;
      unstring_statement;
      usage_clause;
      use_directive_format_i;
      use_directive_format_ii;
      use_directive_format_iii;
      value_clause_format_ii;
      varying_phrase;
      vsam_i_o_control_entries;
      w_phrase;
      write_statement_format_i;
      data_description_entry_format_ii;
      data_description_entry_format_iii;
      literal;
      figurative_constant;
      usage_keyword;
    ]

    let lexical = [
      e;
      ws;
      quoted_pseudo_text;
      id;
      cobol_word;
      numeric;
      nonnumeric;
      integer;
      level_number;
      sixsix;
      eighteight;
      picture_string;
      comment_entry;
      lparen;
      rparen;
      ast;
      astast;
      plus_;
      minus;
      dot;
      slash;
      colon;
      lt;
      le;
      eq;
      gt;
      ge;
      accept_kw;
      access_kw;
      add_kw;
      address_kw;
      advancing_kw;
      after_kw;
      all_kw;
      alphabet_kw;
      alphabetic_kw;
      alphabetic_lower_kw;
      alphabetic_upper_kw;
      alphanumeric_kw;
      alphanumeric_edited_kw;
      also_kw;
      alter_kw;
      alternate_kw;
      and_kw;
      any_kw;
      apply_kw;
      are_kw;
      area_kw;
      areas_kw;
      ascending_kw;
      assign_kw;
      at_kw;
      author_kw;
      before_kw;
      beginning_kw;
      binary_kw;
      blank_kw;
      block_kw;
      bottom_kw;
      by_kw;
      call_kw;
      cancel_kw;
      character_kw;
      characters_kw;
      class_kw;
      close_kw;
      code_set_kw;
      collating_kw;
      comma_kw;
      common_kw;
      comp_kw;
      comp_1_kw;
      comp_2_kw;
      comp_3_kw;
      comp_4_kw;
      comp_5_kw;
      computational_kw;
      computational_1_kw;
      computational_2_kw;
      computational_3_kw;
      computational_4_kw;
      computational_5_kw;
      compute_kw;
      configuration_kw;
      contains_kw;
      content_kw;
      continue_kw;
      converting_kw;
      copy_kw;
      corr_kw;
      corresponding_kw;
      count_kw;
      currency_kw;
      data_kw;
      date_kw;
      date_compiled_kw;
      date_written_kw;
      day_kw;
      day_of_week_kw;
      dbcs_kw;
      debugging_kw;
      debug_item_kw;
      decimal_point_kw;
      declaratives_kw;
      delete_kw;
      delimited_kw;
      delimiter_kw;
      depending_kw;
      descending_kw;
      display_kw;
      display_1_kw;
      divide_kw;
      division_kw;
      down_kw;
      duplicates_kw;
      dynamic_kw;
      ebcdic_kw;
      egcs_kw;
      else_kw;
      end_kw;
      end_add_kw;
      end_call_kw;
      end_compute_kw;
      end_delete_kw;
      end_divide_kw;
      end_evaluate_kw;
      end_if_kw;
      ending_kw;
      end_multiply_kw;
      end_of_page_kw;
      end_perform_kw;
      end_read_kw;
      end_return_kw;
      end_rewrite_kw;
      end_search_kw;
      end_start_kw;
      end_string_kw;
      end_subtract_kw;
      end_unstring_kw;
      end_write_kw;
      entry_kw;
      environment_kw;
      eop_kw;
      equal_kw;
      error_kw;
      evaluate_kw;
      every_kw;
      examine_kw;
      exception_kw;
      exit_kw;
      extend_kw;
      external_kw;
      false_kw;
      fd_kw;
      file_kw;
      file_control_kw;
      filler_kw;
      first_kw;
      footing_kw;
      for_kw;
      from_kw;
      giving_kw;
      global_kw;
      go_kw;
      goback_kw;
      greater_kw;
      high_value_kw;
      high_values_kw;
      id_kw;
      identification_kw;
      if_kw;
      in_kw;
      index_kw;
      indexed_kw;
      initial_kw;
      initialize_kw;
      input_kw;
      input_output_kw;
      inspect_kw;
      installation_kw;
      into_kw;
      invalid_kw;
      i_o_kw;
      i_o_control_kw;
      is_kw;
      just_kw;
      justified_kw;
      kanji_kw;
      key_kw;
      label_kw;
      leading_kw;
      left_kw;
      length_kw;
      less_kw;
      linage_kw;
      linage_counter_kw;
      line_kw;
      lines_kw;
      linkage_kw;
      lock_kw;
      low_value_kw;
      low_values_kw;
      memory_kw;
      merge_kw;
      mode_kw;
      modules_kw;
      more_labels_kw;
      move_kw;
      multiple_kw;
      multiply_kw;
      native_kw;
      negative_kw;
      next_kw;
      no_kw;
      not_kw;
      null_kw;
      nulls_kw;
      numeric_kw;
      numeric_edited_kw;
      object_computer_kw;
      occurs_kw;
      of_kw;
      off_kw;
      omitted_kw;
      on_kw;
      open_kw;
      optional_kw;
      or_kw;
      order_kw;
      organization_kw;
      other_kw;
      output_kw;
      overflow_kw;
      packed_decimal_kw;
      padding_kw;
      page_kw;
      password_kw;
      perform_kw;
      pic_kw;
      picture_kw;
      pointer_kw;
      position_kw;
      positive_kw;
      procedure_kw;
      procedures_kw;
      proceed_kw;
      program_kw;
      program_id_kw;
      quote_kw;
      quotes_kw;
      random_kw;
      read_kw;
      record_kw;
      recording_kw;
      records_kw;
      redefines_kw;
      reel_kw;
      reference_kw;
      relative_kw;
      release_kw;
      remainder_kw;
      remarks_kw;
      removal_kw;
      renames_kw;
      replacing_kw;
      rerun_kw;
      reserve_kw;
      return_kw;
      return_code_kw;
      reversed_kw;
      rewind_kw;
      rewrite_kw;
      right_kw;
      rounded_kw;
      run_kw;
      same_kw;
      sd_kw;
      search_kw;
      section_kw;
      security_kw;
      segment_limit_kw;
      select_kw;
      sentence_kw;
      separate_kw;
      sequence_kw;
      sequential_kw;
      set_kw;
      shift_in_kw;
      shift_out_kw;
      sign_kw;
      size_kw;
      sort_kw;
      sort_control_kw;
      sort_core_size_kw;
      sort_file_size_kw;
      sort_merge_kw;
      sort_message_kw;
      sort_mode_size_kw;
      sort_return_kw;
      source_computer_kw;
      space_kw;
      spaces_kw;
      special_names_kw;
      standard_kw;
      standard_1_kw;
      standard_2_kw;
      start_kw;
      status_kw;
      stop_kw;
      string_kw;
      subtract_kw;
      suppress_kw;
      symbolic_kw;
      sync_kw;
      synchronized_kw;
      tally_kw;
      tallying_kw;
      tape_kw;
      test_kw;
      than_kw;
      then_kw;
      through_kw;
      thru_kw;
      time_kw;
      times_kw;
      to_kw;
      top_kw;
      trailing_kw;
      true_kw;
      unit_kw;
      unstring_kw;
      until_kw;
      up_kw;
      upon_kw;
      usage_kw;
      use_kw;
      using_kw;
      value_kw;
      values_kw;
      varying_kw;
      when_kw;
      when_compiled_kw;
      with_kw;
      words_kw;
      working_storage_kw;
      write_kw;
      write_only_kw;
      zero_kw;
      zeroes_kw;
      zeros_kw;
    ]

  let labels = [
    u
  ]

  let early_stop =
    let f l0 l1 l2 l3 =
      let open Early_stop in
      match l0, l1, l2, l3 with
      | T.Symbol.Code c0, T.Symbol.Code c1, T.Symbol.Code c2, T.Symbol.Code c3 ->
        c0 = code "." &&
        not ((code "0" <= c1 && c1 <= code "9") ||
             ((c1 = code "e" || c1 = code "E") &&
              ((code "0" <= c2 && c2 <= code "9") ||
               ((c2 = code "+" || c2 = code "-") &&
                (code "0" <= c3 && c3 <= code "9")))))
      | _ -> false
    in
    [
      (integer, f);
    ]
    let start = start

    let parser =
      unextend s u @@ with_ws (T.Vars.of_list lexical) R.(opt (var ws)) @@ Production.[
          make (u, start) R.(var cobol_source_program * var e * var e');
          make (u, e') R.(var e * var e' + null);

          make (u, mode) (var cobol_word);

          make (u, priority_number) (var numeric);

          make (u, user_defined_word) (var cobol_word);

          make (u, system_name) (var cobol_word);

          make (u, alphabetic_user_defined_word) (var cobol_word);

          make (u, copy_directive_without_period) R.(var copy_kw * (var text_name + var literal) * opt ((var of_kw + var in_kw) * (var library_name + var literal)) * opt (var suppress_kw) * opt (var replacing_kw * plus (var copy_operand * var by_kw * var copy_operand)));

          make (u, statements) R.(var accept_statement * opt (var statements) + var add_statement * opt ((var statements + var end_add_kw * opt (var statements) + var size_error_phrases * (var end_add_kw * opt (opt (var statements))))) + var alter_statement * opt (var statements) + var call_statement_format_i * opt ((var statements + var end_call_kw * opt (var statements) + var on_overflow * opt (var end_call_kw * opt (var statements)))) + var call_statement_format_ii * opt ((var statements + var end_call_kw * opt (var statements) + var exception_phrases * opt (var end_call_kw * opt (var statements)))) + var cancel_statement * opt (var statements) + var close_statement * opt (var statements) + var compute_statement * opt ((var statements + var end_compute_kw * opt (var statements) + var size_error_phrases * opt (var end_compute_kw * opt (var statements)))) + var continue_statement * opt (var statements) + var delete_statement * opt ((var statements + var end_delete_kw * opt (var statements) + var invalid_key_phrases * opt (var end_delete_kw * opt (var statements)))) + var display_statement * opt (var statements) + var divide_statement * opt ((var statements + var end_divide_kw * opt (var statements) + var size_error_phrases * opt (var end_divide_kw * opt (var statements)))) + var entry_statement * opt (var statements) + var evaluate_statement * opt (var end_evaluate_kw * opt (var statements)) + var exit_statement * opt (var statements) + var exit_program_statement * opt (var statements) + var goback_statement * opt (var statements) + var go_to_statement * opt (var statements) + var if_statement * opt (var end_if_kw * opt (var statements)) + var initialize_statement * opt (var statements) + var inspect_statement * opt (var statements) + var merge_statement * opt (var statements) + var move_statement * opt (var statements) + var multiply_statement * opt ((var statements + var end_multiply_kw * opt (var statements) + var size_error_phrases * opt (var end_multiply_kw * opt (var statements)))) + var open_statement * opt (var statements) + var perform_statement * opt (var statements) + var read_statement_format_i * opt ((var statements + var end_read_kw * opt (var statements) + var at_end_phrases * opt (var end_read_kw * opt (var statements)))) + var read_statement_format_ii * opt ((var statements + var end_read_kw * opt (var statements) + var invalid_key_phrases * opt (var end_read_kw * opt (var statements)))) + var release_statement * opt (var statements) + var return_statement * opt (var end_return_kw * opt (var statements)) + var rewrite_statement * opt ((var statements + var end_rewrite_kw * opt (var statements) + var invalid_key_phrases * opt (var end_rewrite_kw * opt (var statements)))) + var search_statement * opt (var end_search_kw * opt (var statements)) + var set_statement * opt (var statements) + var sort_statement * opt (var statements) + var start_statement * opt ((var statements + var end_start_kw * opt (var statements) + var invalid_key_phrases * opt (var end_start_kw * opt (var statements)))) + var stop_statement * opt (var statements) + var string_statement * opt ((var statements + var end_string_kw * opt (var statements) + var overflow_phrases * opt (var end_string_kw * opt (var statements)))) + var subtract_statement * opt ((var statements + var end_subtract_kw * opt (var statements) + var size_error_phrases * opt (var end_subtract_kw * opt (var statements)))) + var unstring_statement * opt ((var statements + var end_unstring_kw * opt (var statements) + var overflow_phrases * opt (var end_unstring_kw * opt (var statements)))) + var write_statement_format_i * opt ((var statements + var end_write_kw * opt (var statements) + var write_before_after * opt (var statements + var end_write_kw * opt (var statements) + var w_phrase * opt (var end_write_kw * opt (var statements))) + var invalid_key_phrases * opt (var end_write_kw * opt (var statements)))) + var examine_statement * opt (var statements) + var copy_directive * opt (var statements));

          make (u, overflow_phrases) R.(var on_overflow + var not_on_overflow + var on_overflow * var not_on_overflow);

          make (u, invalid_key_phrases) R.(var invalid_key + var not_invalid_key + var invalid_key * var not_invalid_key);

          make (u, at_end_phrases) R.(var at_end + var not_at_end + var at_end * var not_at_end);

          make (u, exception_phrases) R.(var on_exception + var not_on_exception + var on_exception * var not_on_exception);

          make (u, size_error_phrases) R.(var on_size_error + var not_on_size_error + var on_size_error * var not_on_size_error);

          make (u, examine_statement) R.(var examine_kw * var identifier * var tallying_kw * (var all_kw + var leading_kw) * var literal);

          make (u, not_at_eop) R.(var not_kw * opt (var at_kw) * (var end_of_page_kw + var eop_kw) * var series_of_imperative_statements);

          make (u, at_eop) R.(opt (var at_kw) * (var end_of_page_kw + var eop_kw) * var series_of_imperative_statements);

          make (u, write_before_after) R.(opt ((var before_kw + var after_kw) * opt (var advancing_kw) * ((var identifier + var integer) * opt ((var line_kw + var lines_kw)) + var mnemonic_name + var page_kw)));

          make (u, not_at_end) R.(var not_kw * opt (var at_kw) * var end_kw * var series_of_imperative_statements);

          make (u, at_end) R.(opt (var at_kw) * var end_kw * var series_of_imperative_statements);

          make (u, inspect_replacing_phrase) R.(var replacing_kw * plus ((var characters_kw * var by_kw * (var identifier + var literal) * star (var before_after_phrase) + (var all_kw + var leading_kw + var first_kw) * plus ((var identifier + var literal) * var by_kw * (var identifier + var literal) * star (var before_after_phrase)))));

          make (u, inspect_tallying_phrase) R.(var tallying_kw * plus ((var identifier * var for_kw * plus (var characters_kw * star (var before_after_phrase) + (var all_kw + var leading_kw) * plus ((var identifier + var literal) * star (var before_after_phrase))))));

          make (u, before_after_phrase) R.((var before_kw + var after_kw) * opt (var initial_kw) * (var identifier + var literal));

          make (u, when_other_phrase) R.(var when_kw * var other_kw * var series_of_imperative_statements);

          make (u, when_phrase) R.(plus (var when_kw * var e_phrase * star (var also_kw * var e_phrase)) * var series_of_imperative_statements);

          make (u, e_phrase) R.(var any_kw + var condition + var true_kw + var false_kw + opt (var not_kw) * (var identifier + var literal + var arithmetic_expression) * opt ((var through_kw + var thru_kw) * (var identifier + var literal + var arithmetic_expression)));

          make (u, invalid_key) R.(var invalid_kw * opt (var key_kw) * var series_of_imperative_statements);

          make (u, not_invalid_key) R.(var not_kw * var invalid_kw * opt (var key_kw) * var series_of_imperative_statements);

          make (u, not_on_exception) R.(var not_kw * opt (var on_kw) * var exception_kw * var series_of_imperative_statements);

          make (u, on_exception) R.(opt (var on_kw) * var exception_kw * var series_of_imperative_statements);

          make (u, not_on_overflow) R.(var not_kw * opt (var on_kw) * var overflow_kw * var series_of_imperative_statements);

          make (u, on_overflow) R.(opt (var on_kw) * var overflow_kw * var series_of_imperative_statements);

          make (u, not_on_size_error) R.(var not_kw * opt (var on_kw) * var size_kw * var error_kw * var series_of_imperative_statements);

          make (u, on_size_error) R.(opt (var on_kw) * var size_kw * var error_kw * var series_of_imperative_statements);

          make (u, use_directive) R.(var use_directive_format_i + var use_directive_format_ii + var use_directive_format_iii);

          make (u, subtract_statement) R.(var subtract_statement_format_i + var subtract_statement_format_ii + var subtract_statement_format_iii);

          make (u, set_statement) R.(var set_statement_format_i + var set_statement_format_ii + var set_statement_format_iii + var set_statement_format_iv + var set_statement_format_v);

          make (u, search_statement) R.(var search_statement_format_i + var search_statement_format_ii);

          make (u, perform_statement) R.(var perform_statement_format_i + var perform_statement_format_ii + var perform_statement_format_iii + var perform_statement_format_iv);

          make (u, open_statement) R.(var open_statement_format_i + var open_statement_format_ii);

          make (u, multiply_statement) R.(var multiply_statement_format_i + var multiply_statement_format_ii);

          make (u, move_statement) R.(var move_statement_format_i + var move_statement_format_ii);

          make (u, inspect_statement) R.(var inspect_statement_format_i + var inspect_statement_format_ii + var inspect_statement_format_iii + var inspect_statement_format_iv);

          make (u, go_to_statement) R.(var go_to_statement_format_i + var go_to_statement_format_ii + var altered_go_to + var go_to_statement_format_iv);

          make (u, divide_statement) R.(var divide_statement_format_i + var divide_statement_format_ii + var divide_statement_format_iii + var divide_statement_format_iv + var divide_statement_format_v);

          make (u, close_statement) R.(var close_statement_format_i + var close_statement_format_ii);

          make (u, add_statement) R.(var add_statement_format_i + var add_statement_format_ii + var add_statement_format_iii);

          make (u, accept_statement) R.(var accept_statement_format_i + var accept_statement_format_ii);

          make (u, call_using_phrase) R.(var using_kw * plus (opt (opt (var by_kw) * var reference_kw) * plus ((var identifier + var address_kw * var of_kw * var identifier + var file_name)) + opt (var by_kw) * var content_kw * plus (opt (var length_kw * var of_kw) * var identifier + var address_kw * var of_kw * var identifier + var literal)));

          make (u, altered_go_to) R.(var go_kw * opt (var to_kw));

          make (u, occurs_clause) R.(var occurs_clause_format_i + var occurs_clause_format_ii);

          make (u, data_clauses) R.(var blank_when_zero_clause + var external_clause + var global_clause + var justified_clause + var occurs_clause + var picture_clause + var sign_clause + var synchronized_clause + var usage_clause + var value_clause_format_ii + var renames_clause);

          make (u, data_description_entry) R.(var data_description_entry_format_i + var data_description_entry_format_ii + var data_description_entry_format_iii + var copy_directive);

          make (u, code_set_clause) R.(var code_set_kw * opt (var is_kw) * var alphabet_name);

          make (u, recording_mode_clause) R.(var recording_kw * opt (var mode_kw) * opt (var is_kw) * var mode);

          make (u, linage_area_clause) R.(opt (opt (var with_kw) * var footing_kw * opt (var at_kw) * (var data_name + var integer)) * opt (opt (var lines_kw) * opt (var at_kw) * var top_kw * (var data_name + var integer)) * opt (opt (var lines_kw) * opt (var at_kw) * var bottom_kw * (var data_name + var integer)));

          make (u, linage_clause) R.(var linage_kw * opt (var is_kw) * (var qualified_data_name + var integer) * opt (var lines_kw) * var linage_area_clause);

          make (u, data_records_clause) R.(var data_kw * (var record_kw + var records_kw) * opt ((var is_kw + var are_kw)) * plus (var qualified_data_name));

          make (u, value_of_clause) R.(var value_kw * var of_kw * plus (var system_name * opt (var is_kw) * (var qualified_data_name + var literal)));

          make (u, label_records_clause) R.(var label_kw * (var record_kw + var records_kw) * opt ((var is_kw + var are_kw)) * (var standard_kw + var omitted_kw + plus (var qualified_data_name)));

          make (u, record_varying_clause) R.(opt (var is_kw) * var varying_kw * opt (var in_kw) * opt (var size_kw) * opt (opt (var from_kw) * var integer) * opt (var to_kw * var integer) * opt (var characters_kw));

          make (u, record_clause) R.(var record_kw * (opt (var contains_kw) * var integer * opt (var characters_kw) + opt (var contains_kw) * var integer * var to_kw * var integer * opt (var characters_kw) + var record_varying_clause * opt (var depending_kw * opt (var on_kw) * var qualified_data_name)));

          make (u, block_contains_clause) R.(var block_kw * opt (var contains_kw) * opt (var integer * var to_kw) * var integer * opt ((var characters_kw + var records_kw + var record_kw)));

          make (u, global_clause) R.(opt (var is_kw) * var global_kw);

          make (u, external_clause) R.(opt (var is_kw) * var external_kw);

          make (u, file_clauses) R.(plus (var external_clause + var global_clause + var block_contains_clause + var record_clause + var label_records_clause + var value_of_clause + var data_records_clause + var linage_clause + var recording_mode_clause + var code_set_clause));

          make (u, file_description_entry) R.((var fd_kw + var sd_kw) * var file_name * opt (var file_clauses) * var dot);

          make (u, status_clause) R.(opt (var file_kw) * var status_kw * opt (var is_kw) * var qualified_data_name * opt (var qualified_data_name));

          make (u, relative_key) R.(var relative_kw * opt (var key_kw) * opt (var is_kw) * var qualified_data_name);

          make (u, record_key) R.(var record_kw * opt (var key_kw) * opt (var is_kw) * var qualified_data_name * opt (var password_clause) * star (var idx_entry));

          make (u, password_clause) R.(var password_kw * opt (var is_kw) * var qualified_data_name);

          make (u, key_clause) R.(var record_key + var relative_key);

          make (u, access_mode_clause) R.(opt (var access_kw * opt (var mode_kw) * opt (var is_kw)) * (var sequential_kw + var random_kw + var dynamic_kw));

          make (u, record_delimiter_clause) R.(var record_kw * var delimiter_kw * opt (var is_kw) * (var standard_1_kw + var assignment_name));

          make (u, padding_character_clause) R.(var padding_kw * opt (var character_kw) * opt (var is_kw) * (var qualified_data_name + var literal));

          make (u, organisation_clause) R.(opt (var organization_kw * opt (var is_kw)) * (var sequential_kw + var indexed_kw + var relative_kw));

          make (u, reserve_clause) R.(var reserve_kw * var integer * opt ((var area_kw + var areas_kw)));

          make (u, file_control_clauses) R.(var reserve_clause + var organisation_clause + var padding_character_clause + var record_delimiter_clause + var access_mode_clause + var key_clause + var password_clause + var status_clause);

          make (u, assign_clause) R.(var assign_kw * opt (var to_kw) * plus ((var assignment_name + var literal)));

          make (u, select_clause) R.(var select_kw * opt (var optional_kw) * var file_name);

          make (u, file_control_entry) R.(var select_clause * var assign_clause * star (var file_control_clauses) * var dot);

          make (u, special_names_clauses) R.(plus (var environment_clause + var alphabet_clause + var symbolic_clause + var class_clause + var currency_clause));

          make (u, currency_clause) R.(var currency_kw * opt (var sign_kw) * opt (var is_kw) * var literal + var decimal_point_kw * opt (var is_kw) * var comma_kw + var currency_kw * opt (var sign_kw) * opt (var is_kw) * var literal * var decimal_point_kw * opt (var is_kw) * var comma_kw);

          make (u, class_clause) R.(var class_kw * var class_name * opt (var is_kw) * plus (var literal * opt ((var through_kw + var thru_kw) * var literal)));

          make (u, symbolic_clause) R.(var symbolic_kw * opt (var characters_kw) * plus (plus (var symbolic_character) * opt ((var are_kw + var is_kw)) * plus (var integer)) * opt (var in_kw * var alphabet_name));

          make (u, alphabet_clause) R.(var alphabet_kw * var alphabet_name * opt (var is_kw) * (var standard_1_kw + var standard_2_kw + var native_kw + var ebcdic_kw + plus (var literal * opt ((var through_kw + var thru_kw) * var literal + plus (var also_kw * var literal)))));

          make (u, environment_clause) R.(var environment_name * opt (var is_kw) * var mnemonic_name + var environment_name * (opt (var is_kw) * var mnemonic_name * opt (var snp_entry) + var snp_entry));

          make (u, computer_paragraphs) R.(plus (var source_computer_paragraph + var object_computer_paragraph));

          make (u, sentence) R.(var statements * var dot);

          make (u, series_of_imperative_statements) (var statements);

          make (u, procedure_division_content) R.(opt (var declaratives_kw * var dot * plus (var sect * var dot * var use_directive * var dot * opt (var para)) * var end_kw * var declaratives_kw * var dot) * var para * star (var section));

          make (u, section) R.(var section_name * var section_kw * opt (var priority_number) * var dot * var para);

          make (u, using_phrase) R.(var using_kw * plus (var data_name));

          make (u, record_description_entry) (var data_description_entry);

          make (u, data_division_content) R.(opt (var file_section) * opt (var working_storage) * opt (var linkage_section));

          make (u, file_section) R.(var file_kw * var section_kw * var dot * star (var file_description_entry * plus (var record_description_entry)));

          make (u, working_storage) R.(var working_storage_kw * var section_kw * var dot * star (var data_description_entry));

          make (u, linkage_section) R.(var linkage_kw * var section_kw * var dot * star (var data_description_entry));

          make (u, environment_division_content) R.(opt (var configuration_section) * opt (var input_output_section));

          make (u, identification_division_content) R.(plus ((var author_kw * opt (var dot) * opt (var comment_entry) + var installation_kw * opt (var dot) * opt (var comment_entry) + var date_written_kw * opt (var dot) * opt (var comment_entry) + var date_compiled_kw * opt (var dot) * opt (var comment_entry) + var security_kw * opt (var dot) * opt (var comment_entry) + var remarks_kw * var dot * opt (var comment_entry))));

          make (u, copy_operand) R.(var quoted_pseudo_text + var identifier + var literal + var cobol_word);

          make (u, abbreviation_rest) R.(plus ((var and_kw + var or_kw) * opt (var not_kw) * opt (var relational_operator) * (var object_ + var lparen * var object_ * var abbreviation_rest * var rparen)));

          make (u, object_) (var arithmetic_expression);

          make (u, relational_operator) R.(opt ((var is_kw + var not_kw + var is_kw * var not_kw + var not_kw * var is_kw)) * (var greater_kw * opt (var than_kw) + var gt + var less_kw * opt (var than_kw) + var lt + var equal_kw * opt (var to_kw) + var eq + var greater_kw * opt (var than_kw) * var or_kw * var equal_kw * opt (var to_kw) + var ge + var less_kw * opt (var than_kw) * var or_kw * var equal_kw * opt (var to_kw) + var le));

          make (u, operand) (var arithmetic_expression);

          make (u, simple_condition) R.(var class_condition + var condition_name_condition + var relation_condition + var sign_condition + var switch_status_condition + var lparen * var condition * var rparen);

          make (u, combinable_condition) R.(var simple_condition + var negated_simple_condition + var abbreviated_combined_relation_condition);

          make (u, condition) R.(var combinable_condition + var combined_condition);

          make (u, basis) R.(var identifier + var literal + var lparen * var arithmetic_expression * var rparen);

          make (u, power) R.(opt ((var plus_ + var minus)) * var basis * star (var astast * var basis));

          make (u, times_div) R.(var power * star ((var ast + var slash) * var power));

          make (u, arithmetic_expression) R.(var times_div * star ((var plus_ + var minus) * var times_div));

          make (u, special_register) R.(var address_kw * var of_kw * var data_name + var debug_item_kw + var length_kw * var of_kw * var identifier + var return_code_kw + var shift_out_kw + var shift_in_kw + var sort_control_kw + var sort_core_size_kw + var sort_file_size_kw + var sort_message_kw + var sort_mode_size_kw + var sort_return_kw + var tally_kw + var when_compiled_kw);

          make (u, assignment_name) (var system_name);

          make (u, environment_name) (var system_name);

          make (u, computer_name) (var system_name);

          make (u, section_name) R.(var user_defined_word + var integer);

          make (u, paragraph_name) R.(var user_defined_word + var integer);

          make (u, text_name) (var user_defined_word);

          make (u, program_name) (var user_defined_word);

          make (u, library_name) (var user_defined_word);

          make (u, symbolic_character) (var alphabetic_user_defined_word);

          make (u, record_name) (var qualified_data_name);

          make (u, mnemonic_name) (var alphabetic_user_defined_word);

          make (u, index_name) (var alphabetic_user_defined_word);

          make (u, file_name) (var alphabetic_user_defined_word);

          make (u, data_name) (var alphabetic_user_defined_word);

          make (u, condition_name) (var alphabetic_user_defined_word);

          make (u, class_name) (var alphabetic_user_defined_word);

          make (u, alphabet_name) (var alphabetic_user_defined_word);

          make (u, length) (var arithmetic_expression);

          make (u, leftmost_character_position) (var arithmetic_expression);

          make (u, condition_name_reference) R.(var condition_name_in_data_division + var condition_name_in_special_names_paragraph);

          make (u, subscript) R.(var integer + var identifier * opt ((var plus_ + var minus) * var integer) + var index_name * opt ((var plus_ + var minus) * var integer) + (var plus_ + var minus) * var integer);

          make (u, qualified_data_name) R.(var data_name * star ((var in_kw + var of_kw) * var data_name) * opt ((var in_kw + var of_kw) * var file_name));

          make (u, identifier) R.(var identifier_format_i + var identifier_format_ii + var special_register);

          make (u, procedure_name) R.(var reference_to_procedure_division_name_format_i + var reference_to_procedure_division_name_format_ii);

          make (u, abbreviated_combined_relation_condition) R.(var relation_condition * var abbreviation_rest + var arithmetic_expression * var relational_operator * var lparen * opt (var not_kw) * var arithmetic_expression * var abbreviation_rest * var rparen + var arithmetic_expression * var lparen * opt (var not_kw) * opt (var relational_operator) * var arithmetic_expression * var abbreviation_rest * var rparen);

          make (u, accept_statement_format_i) R.(var accept_kw * var identifier * opt (var from_kw * (var mnemonic_name + var environment_name)));

          make (u, accept_statement_format_ii) R.(var accept_kw * var identifier * var from_kw * (var date_kw + var day_kw + var day_of_week_kw + var time_kw));

          make (u, add_statement_format_i) R.(var add_kw * plus ((var identifier + var literal)) * var to_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, add_statement_format_ii) R.(var add_kw * plus ((var identifier + var literal)) * opt (var to_kw) * (var identifier + var literal) * var giving_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, add_statement_format_iii) R.(var add_kw * (var corresponding_kw + var corr_kw) * var identifier * var to_kw * var identifier * opt (var rounded_kw));

          make (u, after_phrase) R.(star (var after_kw * (var identifier + var index_name) * var from_kw * (var identifier + var index_name + var literal) * var by_kw * (var identifier + var literal) * var until_kw * var condition));

          make (u, alter_statement) R.(var alter_kw * plus (var procedure_name * var to_kw * opt (var proceed_kw * var to_kw) * var procedure_name));

          make (u, blank_when_zero_clause) R.(var blank_kw * opt (var when_kw) * (var zero_kw + var zeros_kw + var zeroes_kw));

          make (u, call_statement_format_i) R.(var call_kw * (var identifier + var literal) * opt (var call_using_phrase * opt (var copy_directive_without_period)));

          make (u, call_statement_format_ii) R.(var call_kw * (var identifier + var literal) * opt (var call_using_phrase * opt (var copy_directive_without_period)));

          make (u, cancel_statement) R.(var cancel_kw * plus ((var identifier + var literal)));

          make (u, class_condition) R.(var identifier * opt ((var is_kw + var not_kw + var is_kw * var not_kw + var not_kw * var is_kw)) * (var numeric_kw + var alphabetic_kw + var alphabetic_lower_kw + var alphabetic_upper_kw + var class_name + var dbcs_kw + var kanji_kw));

          make (u, close_statement_format_i) R.(var close_kw * plus (var file_name * opt ((var reel_kw + var unit_kw) * opt (opt (var for_kw) * var removal_kw + opt (var with_kw) * var no_kw * var rewind_kw) + opt (var with_kw) * (var no_kw * var rewind_kw + var lock_kw))));

          make (u, close_statement_format_ii) R.(var close_kw * plus (var file_name * opt (opt (var with_kw) * var lock_kw)));

          make (u, combined_condition) R.(var combinable_condition * plus ((var and_kw + var or_kw) * var combinable_condition));

          make (u, compute_statement) R.(var compute_kw * plus (var identifier * opt (var rounded_kw)) * (var eq + var equal_kw) * var arithmetic_expression);

          make (u, condition_name_condition) (var condition_name_reference);

          make (u, condition_name_in_data_division) R.(var condition_name * star ((var in_kw + var of_kw) * var data_name) * opt ((var in_kw + var of_kw) * var file_name) * star (var lparen * var subscript * var rparen));

          make (u, condition_name_in_special_names_paragraph) R.(var condition_name * star ((var in_kw + var of_kw) * var mnemonic_name));

          make (u, configuration_section) R.(var configuration_kw * var section_kw * var dot * opt (var computer_paragraphs) * opt (var special_names_paragraph));

          make (u, continue_statement) (var continue_kw);

          make (u, copy_directive) R.(var copy_directive_without_period * var dot);

          make (u, data_description_entry_format_i) R.(var level_number * opt ((var data_name + var filler_kw)) * opt (var redefines_clause) * star (var data_clauses) * opt (var dot));

          make (u, delete_statement) R.(var delete_kw * var file_name * opt (var record_kw));

          make (u, display_statement) R.(var display_kw * plus (var identifier + var literal) * opt (var upon_kw * (var mnemonic_name + var environment_name)) * opt (opt (var with_kw) * var no_kw * var advancing_kw));

          make (u, divide_statement_format_i) R.(var divide_kw * (var identifier + var literal) * var into_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, divide_statement_format_ii) R.(var divide_kw * (var identifier + var literal) * var into_kw * (var identifier + var literal) * var giving_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, divide_statement_format_iii) R.(var divide_kw * (var identifier + var literal) * var by_kw * (var identifier + var literal) * var giving_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, divide_statement_format_iv) R.(var divide_kw * (var identifier + var literal) * var into_kw * (var identifier + var literal) * var giving_kw * var identifier * opt (var rounded_kw) * var remainder_kw * var identifier);

          make (u, divide_statement_format_v) R.(var divide_kw * (var identifier + var literal) * var by_kw * (var identifier + var literal) * var giving_kw * var identifier * opt (var rounded_kw) * var remainder_kw * var identifier);

          make (u, entry_statement) R.(var entry_kw * var literal * opt (var using_kw * plus (var data_name)));

          make (u, evaluate_statement) R.(var evaluate_kw * (var identifier + var literal + var arithmetic_expression + var condition + var true_kw + var false_kw) * star (var also_kw * (var identifier + var literal + var arithmetic_expression + var condition + var true_kw + var false_kw)) * plus (var when_phrase) * opt (var when_other_phrase));

          make (u, exit_program_statement) R.(var exit_kw * var program_kw);

          make (u, exit_statement) (var exit_kw);

          make (u, file_control_paragraph) R.(var file_control_kw * var dot * star (var file_control_entry));

          make (u, go_to_statement_format_i) R.(var go_kw * opt (var to_kw) * var procedure_name);

          make (u, go_to_statement_format_ii) R.(var go_kw * opt (var to_kw) * plus (var procedure_name) * var depending_kw * opt (var on_kw) * var identifier);

          make (u, go_to_statement_format_iv) R.(var go_kw * opt (var to_kw) * var more_labels_kw);

          make (u, goback_statement) (var goback_kw);

          make (u, i_o_control_paragraph) R.(var i_o_control_kw * var dot * opt (plus ((var qsam_or_sam_i_o_control_entries + var vsam_i_o_control_entries)) * var dot) * opt (var sort_merge_i_o_control_entries * var dot));

          make (u, identifier_format_i) R.(var qualified_data_name * opt (var lparen * plus (var subscript) * var rparen) * opt (var lparen * var leftmost_character_position * var colon * opt (var length) * var rparen));

          make (u, identifier_format_ii) R.(var linage_counter_kw * opt ((var in_kw + var of_kw) * var file_name));

          make (u, idx_entry) R.(var alternate_kw * opt (var record_kw) * opt (var key_kw) * opt (var is_kw) * var qualified_data_name * opt (var password_kw * opt (var is_kw) * var qualified_data_name) * opt (opt (var with_kw) * var duplicates_kw));

          make (u, if_statement) R.(var if_kw * var condition * opt (var then_kw) * (var statements + var next_kw * var sentence_kw) * opt (var else_kw * (var statements + var next_kw * var sentence_kw)));

          make (u, initialize_statement) R.(var initialize_kw * plus (var identifier) * opt (var replacing_kw * plus ((var alphabetic_kw + var alphanumeric_kw + var numeric_kw + var alphanumeric_edited_kw + var numeric_edited_kw + var dbcs_kw + var egcs_kw) * opt (var data_kw) * var by_kw * (var identifier + var literal))));

          make (u, input_output_section) R.(var input_output_kw * var section_kw * var dot * opt (var file_control_paragraph) * opt (var i_o_control_paragraph));

          make (u, inspect_statement_format_i) R.(var inspect_kw * var identifier * var inspect_tallying_phrase);

          make (u, inspect_statement_format_ii) R.(var inspect_kw * var identifier * var inspect_replacing_phrase);

          make (u, inspect_statement_format_iii) R.(var inspect_kw * var identifier * var inspect_tallying_phrase * var inspect_replacing_phrase);

          make (u, inspect_statement_format_iv) R.(var inspect_kw * var identifier * var converting_kw * (var identifier + var literal) * var to_kw * (var identifier + var literal) * star (var before_after_phrase));

          make (u, justified_clause) R.((var justified_kw + var just_kw) * opt (var right_kw));

          make (u, merge_statement) R.(var merge_kw * var file_name * plus (opt (var on_kw) * (var ascending_kw + var descending_kw) * opt (var key_kw) * plus (var qualified_data_name)) * opt (opt (var collating_kw) * var sequence_kw * opt (var is_kw) * var alphabet_name) * var using_kw * var file_name * plus (var file_name) * (var output_kw * var procedure_kw * opt (var is_kw) * var procedure_name * opt ((var through_kw + var thru_kw) * var procedure_name) + var giving_kw * plus (var file_name)));

          make (u, move_statement_format_i) R.(var move_kw * (var identifier + var literal) * var to_kw * plus (var identifier));

          make (u, move_statement_format_ii) R.(var move_kw * (var corresponding_kw + var corr_kw) * var identifier * var to_kw * plus (var identifier));

          make (u, multiply_statement_format_i) R.(var multiply_kw * (var identifier + var literal) * var by_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, multiply_statement_format_ii) R.(var multiply_kw * (var identifier + var literal) * var by_kw * (var identifier + var literal) * var giving_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, negated_simple_condition) R.(var not_kw * var condition);

          make (u, cobol_source_program) R.(opt (var ws) * ((var identification_kw + var id_kw) * var division_kw * var dot * var program_id_kw * (var dot) * var program_name * opt (opt (var is_kw) * var initial_kw * opt (var program_kw)) * opt (var dot) * opt (var identification_division_content) * opt (var environment_kw * var division_kw * var dot * var environment_division_content) * opt (var data_kw * var division_kw * var dot * var data_division_content) * opt (var procedure_kw * var division_kw * (opt (var using_phrase) * var dot + var copy_directive + var using_kw * star (var data_name) * var copy_directive) * var procedure_division_content) * opt (star (var nested_source_program) * var end_kw * var program_kw * var program_name * var dot)));

          make (u, nested_source_program) R.((var identification_kw + var id_kw) * var division_kw * var dot * var program_id_kw * (var dot) * var program_name * opt (opt (var is_kw) * (var common_kw * opt (var initial_kw) + var initial_kw * opt (var common_kw)) * opt (var program_kw)) * opt (var dot) * opt (var identification_division_content) * opt (var environment_kw * var division_kw * var dot * var environment_division_content) * opt (var data_kw * var division_kw * var dot * var data_division_content) * opt (var procedure_kw * var division_kw * (opt (var using_phrase) * var dot + var copy_directive + var using_kw * star (var data_name) * var copy_directive) * var procedure_division_content) * star (var nested_source_program) * var end_kw * var program_kw * var program_name * var dot);

          make (u, object_computer_paragraph) R.(var object_computer_kw * var dot * opt (var computer_name * opt (var memory_kw * opt (var size_kw) * var integer * (var words_kw + var characters_kw + var modules_kw)) * var ocp_entry * var dot));

          make (u, occurs_clause_format_i) R.(var occurs_kw * var integer * opt (var times_kw) * star ((var ascending_kw + var descending_kw) * opt (var key_kw) * opt (var is_kw) * plus (var data_name)) * opt (var indexed_kw * opt (var by_kw) * plus (var index_name)));

          make (u, occurs_clause_format_ii) R.(var occurs_kw * opt (var integer * var to_kw) * var integer * opt (var times_kw) * var depending_kw * opt (var on_kw) * var qualified_data_name * star ((var ascending_kw + var descending_kw) * opt (var key_kw) * opt (var is_kw) * plus (var data_name)) * opt (var indexed_kw * opt (var by_kw) * plus (var index_name)));

          make (u, ocp_entry) R.(opt (opt (var program_kw) * opt (var collating_kw) * var sequence_kw * opt (var is_kw) * var alphabet_name) * opt (var segment_limit_kw * opt (var is_kw) * var priority_number));

          make (u, open_statement_format_i) R.(var open_kw * plus ((var input_kw * plus (var file_name * opt (var reversed_kw + opt (var with_kw) * var no_kw * var rewind_kw)) + var output_kw * plus (var file_name * opt (opt (var with_kw) * var no_kw * var rewind_kw)) + var i_o_kw * plus (var file_name) + var extend_kw * plus (var file_name))));

          make (u, open_statement_format_ii) R.(var open_kw * plus ((var input_kw * plus (var file_name) + var output_kw * plus (var file_name) + var i_o_kw * plus (var file_name) + var extend_kw * plus (var file_name))));

          make (u, para) R.(opt (star (var sentence) * star (var paragraph_name * var dot * star (var sentence))));

          make (u, perform_statement_format_i) R.(var perform_kw * (var procedure_name * opt ((var through_kw + var thru_kw) * var procedure_name) + opt (var series_of_imperative_statements) * var end_perform_kw));

          make (u, perform_statement_format_ii) R.(var perform_kw * (var procedure_name * opt ((var through_kw + var thru_kw) * var procedure_name) * (var identifier + var integer) * var times_kw + (var identifier + var integer) * var times_kw * opt (var series_of_imperative_statements) * var end_perform_kw));

          make (u, perform_statement_format_iii) R.(var perform_kw * (var procedure_name * opt ((var through_kw + var thru_kw) * var procedure_name) * var test_phrase + var test_phrase * opt (var series_of_imperative_statements) * var end_perform_kw));

          make (u, perform_statement_format_iv) R.(var perform_kw * (var procedure_name * opt ((var through_kw + var thru_kw) * var procedure_name) * var varying_phrase * var after_phrase + var varying_phrase * opt (var series_of_imperative_statements) * var end_perform_kw));

          make (u, picture_clause) R.((var picture_kw + var pic_kw) * opt (var is_kw) * var picture_string);

          make (u, qsam_or_sam_i_o_control_entries) R.(var rerun_kw * opt (var on_kw) * (var assignment_name + var file_name) * opt (var every_kw) * (var integer * var records_kw + var end_kw * opt (var of_kw) * (var reel_kw + var unit_kw)) * opt (var of_kw) * var file_name + var same_kw * opt (var record_kw) * opt (var area_kw) * opt (var for_kw) * var file_name * star (var file_name) + var multiple_kw * var file_kw * opt (var tape_kw) * opt (var contains_kw) * plus (var file_name * opt (var position_kw * var integer)) + var apply_kw * var write_only_kw * opt (var on_kw) * plus (var file_name));

          make (u, read_statement_format_i) R.(var read_kw * var file_name * opt (var next_kw) * opt (var record_kw) * opt (var into_kw * var identifier));

          make (u, read_statement_format_ii) R.(var read_kw * var file_name * opt (var record_kw) * opt (var into_kw * var identifier) * opt (var key_kw * opt (var is_kw) * var qualified_data_name));

          make (u, redefines_clause) R.(var redefines_kw * var data_name);

          make (u, reference_to_procedure_division_name_format_i) R.(var paragraph_name * opt ((var in_kw + var of_kw) * var section_name));

          make (u, reference_to_procedure_division_name_format_ii) (var section_name);

          make (u, relation_condition) R.(var operand * var relational_operator * var operand);

          make (u, release_statement) R.(var release_kw * var record_name * opt (var from_kw * var identifier));

          make (u, renames_clause) R.(var renames_kw * var qualified_data_name * opt ((var through_kw + var thru_kw) * var qualified_data_name));

          make (u, return_statement) R.(var return_kw * var file_name * opt (var record_kw) * opt (var into_kw * var identifier) * var at_end * opt (var not_at_end));

          make (u, rewrite_statement) R.(var rewrite_kw * var record_name * opt (var from_kw * var identifier));

          make (u, search_statement_format_i) R.(var search_kw * var identifier * opt (var varying_kw * (var identifier + var index_name)) * opt (var at_end) * plus (var when_kw * var condition * (var series_of_imperative_statements + var next_kw * var sentence_kw)));

          make (u, search_statement_format_ii) R.(var search_kw * var all_kw * opt (var identifier * var at_end) * var when_kw * (var identifier * opt (var is_kw) * (var equal_kw * opt (var to_kw) + var eq) * (var identifier + var literal + var arithmetic_expression) + var condition_name_reference) * star (var and_kw * (var identifier * opt (var is_kw) * (var equal_kw * opt (var to_kw) + var eq) * (var identifier + var literal + var arithmetic_expression) + var condition_name_reference)) * opt (var series_of_imperative_statements + var next_kw * var sentence_kw));

          make (u, sect) R.(var section_name * var section_kw * opt (var priority_number));

          make (u, set_statement_format_i) R.(var set_kw * plus ((var index_name + var identifier)) * var to_kw * (var index_name + var identifier + var integer));

          make (u, set_statement_format_ii) R.(var set_kw * plus (var index_name) * (var up_kw * var by_kw + var down_kw * var by_kw) * (var identifier + var integer));

          make (u, set_statement_format_iii) R.(var set_kw * plus (plus (var mnemonic_name) * var to_kw * (var on_kw + var off_kw)));

          make (u, set_statement_format_iv) R.(var set_kw * plus (var condition_name_reference) * var to_kw * var true_kw);

          make (u, set_statement_format_v) R.(var set_kw * plus ((var identifier + var address_kw * var of_kw * var identifier)) * var to_kw * (var identifier + var address_kw * var of_kw * var identifier + var null_kw + var nulls_kw));

          make (u, sign_clause) R.(opt (var sign_kw * opt (var is_kw)) * (var leading_kw + var trailing_kw) * opt (var separate_kw * opt (var character_kw)));

          make (u, sign_condition) R.(var operand * opt ((var is_kw + var not_kw + var is_kw * var not_kw + var not_kw * var is_kw)) * (var positive_kw + var negative_kw + var zero_kw));

          make (u, snp_entry) R.(var on_kw * opt (var status_kw) * opt (var is_kw) * var condition * opt (var off_kw * opt (var status_kw) * opt (var is_kw) * var condition) + var off_kw * opt (var status_kw) * opt (var is_kw) * var condition * opt (var on_kw * opt (var status_kw) * opt (var is_kw) * var condition));

          make (u, sort_merge_i_o_control_entries) R.(opt (var rerun_kw * var on_kw * var assignment_name) * plus (var same_kw * (var record_kw + var sort_kw + var sort_merge_kw) * opt (var area_kw) * opt (var for_kw) * var file_name * star (var file_name)));

          make (u, sort_statement) R.(var sort_kw * var file_name * plus (opt (var on_kw) * (var ascending_kw + var descending_kw) * opt (var key_kw) * plus (var qualified_data_name)) * opt (opt (var with_kw) * var duplicates_kw * opt (var in_kw) * opt (var order_kw)) * opt (opt (var collating_kw) * var sequence_kw * opt (var is_kw) * var alphabet_name) * (var using_kw * plus (var file_name) + var input_kw * var procedure_kw * opt (var is_kw) * var procedure_name * opt ((var through_kw + var thru_kw) * var procedure_name)) * (var giving_kw * plus (var file_name) + var output_kw * var procedure_kw * opt (var is_kw) * var procedure_name * opt ((var through_kw + var thru_kw) * var procedure_name)));

          make (u, source_computer_paragraph) R.(var source_computer_kw * var dot * opt (var computer_name * opt (opt (var with_kw) * var debugging_kw * var mode_kw) * var dot));

          make (u, special_names_paragraph) R.(var special_names_kw * var dot * var special_names_clauses * opt (var dot));

          make (u, start_statement) R.(var start_kw * var file_name * opt ((var key_kw * opt (var is_kw) * (var equal_kw * opt (var to_kw) + var eq + var greater_kw * opt (var than_kw) + var gt + var not_kw * var less_kw * opt (var than_kw) + var not_kw * var lt + var greater_kw * opt (var than_kw) * var or_kw * var equal_kw * opt (var to_kw) + var ge) * var qualified_data_name)));

          make (u, stop_statement) R.(var stop_kw * (var run_kw + var literal));

          make (u, string_statement) R.(var string_kw * plus (plus ((var identifier + var literal)) * var delimited_kw * opt (var by_kw) * (var identifier + var literal + var size_kw)) * var into_kw * var identifier * opt (opt (var with_kw) * var pointer_kw * var identifier));

          make (u, subtract_statement_format_i) R.(var subtract_kw * plus ((var identifier + var literal)) * var from_kw * plus (var identifier * opt (var rounded_kw)));

          make (u, subtract_statement_format_ii) R.(var subtract_kw * plus ((var identifier + var literal)) * var from_kw * (var identifier + var literal) * var giving_kw * plus ((var identifier * opt (var rounded_kw))));

          make (u, subtract_statement_format_iii) R.(var subtract_kw * (var corresponding_kw + var corr_kw) * var identifier * var from_kw * var identifier * opt (var rounded_kw));

          make (u, switch_status_condition) (var condition_name_reference);

          make (u, synchronized_clause) R.((var synchronized_kw + var sync_kw) * opt ((var left_kw + var right_kw)));

          make (u, test_phrase) R.(opt (opt (var with_kw) * var test_kw * (var before_kw + var after_kw)) * var until_kw * var condition);

          make (u, unstring_statement) R.(var unstring_kw * var identifier * opt (var delimited_kw * opt (var by_kw) * opt (var all_kw) * (var identifier + var literal) * star (var or_kw * opt (var all_kw) * (var identifier + var literal))) * var into_kw * plus (var identifier * opt (var delimiter_kw * opt (var in_kw) * var identifier) * opt (var count_kw * opt (var in_kw) * var identifier)) * opt (opt (var with_kw) * var pointer_kw * var identifier) * opt (var tallying_kw * opt (var in_kw) * var identifier));

          make (u, usage_clause) R.(opt (var usage_kw * opt (var is_kw)) * var usage_keyword);

          make (u, use_directive_format_i) R.(var use_kw * opt (var global_kw) * var after_kw * opt (var standard_kw) * (var exception_kw + var error_kw) * var procedure_kw * opt (var on_kw) * (plus (var file_name) + (var input_kw + var output_kw + var i_o_kw + var extend_kw)));

          make (u, use_directive_format_ii) R.(var use_kw * opt (var global_kw) * var after_kw * opt (var standard_kw) * opt ((var beginning_kw + var ending_kw)) * opt ((var file_kw + var reel_kw + var unit_kw)) * var label_kw * var procedure_kw * opt (var on_kw) * (plus (var file_name) + (var input_kw + var output_kw + var i_o_kw + var extend_kw)));

          make (u, use_directive_format_iii) R.(var use_kw * opt (var for_kw) * var debugging_kw * opt (var on_kw) * (plus (var procedure_name) + var all_kw * var procedures_kw));

          make (u, value_clause_format_ii) R.((var value_kw * opt (var is_kw) + var values_kw * opt (var are_kw)) * plus (var literal * opt ((var through_kw + var thru_kw) * var literal)));

          make (u, varying_phrase) R.(opt (opt (var with_kw) * var test_kw * (var before_kw + var after_kw)) * var varying_kw * (var identifier + var index_name) * var from_kw * (var identifier + var index_name + var literal) * var by_kw * (var identifier + var literal) * var until_kw * var condition);

          make (u, vsam_i_o_control_entries) R.(var rerun_kw * opt (var on_kw) * (var assignment_name + var file_name) * opt (var every_kw) * var integer * var records_kw * opt (var of_kw) * var file_name + var same_kw * opt (var record_kw) * opt (var area_kw) * opt (var for_kw) * var file_name * star (var file_name));

          make (u, w_phrase) R.(var at_eop + var not_at_eop + var at_eop * var not_at_eop);

          make (u, write_statement_format_i) R.(var write_kw * var record_name * opt (var from_kw * var identifier));

          make (u, data_description_entry_format_ii) R.(var sixsix * var data_name * var renames_clause * opt (var dot));

          make (u, data_description_entry_format_iii) R.(var eighteight * var condition_name * var value_clause_format_ii * opt (var dot));

          make (u, literal) R.(var nonnumeric + var numeric + var integer + var figurative_constant);

          make (u, figurative_constant) R.(var zero_kw + var zeros_kw + var zeroes_kw + var space_kw + var spaces_kw + var high_value_kw + var high_values_kw + var low_value_kw + var low_values_kw + var quote_kw + var quotes_kw + var all_kw * var literal + var null_kw + var nulls_kw);

          make (u, usage_keyword) R.(var binary_kw + var comp_kw + var comp_1_kw + var comp_2_kw + var comp_3_kw + var comp_4_kw + var comp_5_kw + var computational_kw + var computational_1_kw + var computational_2_kw + var computational_3_kw + var computational_4_kw + var computational_5_kw + var display_kw + var display_1_kw + var index_kw + var packed_decimal_kw + var pointer_kw);
        ]

    let scanner =
            let digit = range "0" "9" in
      let letter = R.(range "a" "z" + range "A" "Z") in
      let hexdigit = R.(digit + range "A" "F") in
      let hexnumber = R.(codes "xX" * codes "\"" * plus hexdigit * codes "\"" +
                         codes "xX" * codes "'" * plus hexdigit * codes "'") in
      let nullterminated = R.(codes "zZ" * codes "\"" * star (not_codes "\"\n\r" + text "\"\"" + text "'") * codes "\"" +
                              codes "zZ" * codes "'" * star (not_codes "'\n\r" + text "\"" + text "''") * codes "'") in
      let string = R.(codes "\"" * star (not_codes "\"\n\r" + text "\"\"" + text "'") * codes "\"" +
                      codes "'" * star (not_codes "'\n\r" + text "\"" + text "''") * codes "'") in
      let dbcs = R.(codes "GN" * codes "\"" * star (not_codes "\"\n\r" + text "\"\"" + text "'") * codes "\"" +
                    codes "GN" * codes "'" * star (not_codes "'\n\r" + text "\"" + text "''") * codes "'") in
      let int = R.(opt (codes "+-") * plus digit) in
      let numeric_ = R.(opt (codes "+-") * star digit * codes ".," * plus digit * opt (codes "eE" * opt (codes "+-") * plus digit)) in
      let picture_chars = R.(numeric_ + int + codes "aAbBeEgGnNpPsSvVxXzZcCrRdDbB$/,.()+-") in
      let picture_cardinality = R.(codes "(" * int * codes ")") in
      let sixsix_ = text "66" in
      let eighteight_ = text "88" in
      let level_number_ = R.(opt (codes "+-") * (plus digit)) in
      let id_ = R.(letter * star (letter + digit) * star (plus (codes "-_") * plus (letter + digit))) in
      Production.[
        make (u, id) id_;
        make (u, cobol_word) id_;
        make (u, nonnumeric) R.(string + dbcs + hexnumber + nullterminated);
        make (u, numeric) numeric_;
        make (u, integer) int;
        make (u, level_number) level_number_;
        make (u, sixsix) sixsix_;
        make (u, eighteight) eighteight_;
        make (u, picture_string) R.(plus (plus picture_chars * opt picture_cardinality));
        make (u, comment_entry) R.(star (not_codes "\n\r"));

        make (u, lparen) (text "(");
        make (u, rparen) (text ")");
        make (u, ast) (text "*");
        make (u, astast) (text "**");
        make (u, plus_) (text "+");
        make (u, minus) (text "-");
        make (u, dot) (text ".");
        make (u, slash) (text "/");
        make (u, colon) (text ":");
        make (u, lt) (text "<");
        make (u, le) (text "<=");
        make (u, eq) (text "=");
        make (u, gt) (text ">");
        make (u, ge) (text ">=");
        make (u, accept_kw) (text "ACCEPT");
        make (u, access_kw) (text "ACCESS");
        make (u, add_kw) (text "ADD");
        make (u, address_kw) (text "ADDRESS");
        make (u, advancing_kw) (text "ADVANCING");
        make (u, after_kw) (text "AFTER");
        make (u, all_kw) (text "ALL");
        make (u, alphabet_kw) (text "ALPHABET");
        make (u, alphabetic_kw) (text "ALPHABETIC");
        make (u, alphabetic_lower_kw) (text "ALPHABETIC-LOWER");
        make (u, alphabetic_upper_kw) (text "ALPHABETIC-UPPER");
        make (u, alphanumeric_kw) (text "ALPHANUMERIC");
        make (u, alphanumeric_edited_kw) (text "ALPHANUMERIC-EDITED");
        make (u, also_kw) (text "ALSO");
        make (u, alter_kw) (text "ALTER");
        make (u, alternate_kw) (text "ALTERNATE");
        make (u, and_kw) (text "AND");
        make (u, any_kw) (text "ANY");
        make (u, apply_kw) (text "APPLY");
        make (u, are_kw) (text "ARE");
        make (u, area_kw) (text "AREA");
        make (u, areas_kw) (text "AREAS");
        make (u, ascending_kw) (text "ASCENDING");
        make (u, assign_kw) (text "ASSIGN");
        make (u, at_kw) (text "AT");
        make (u, author_kw) (text "AUTHOR");
        make (u, before_kw) (text "BEFORE");
        make (u, beginning_kw) (text "BEGINNING");
        make (u, binary_kw) (text "BINARY");
        make (u, blank_kw) (text "BLANK");
        make (u, block_kw) (text "BLOCK");
        make (u, bottom_kw) (text "BOTTOM");
        make (u, by_kw) (text "BY");
        make (u, call_kw) (text "CALL");
        make (u, cancel_kw) (text "CANCEL");
        make (u, character_kw) (text "CHARACTER");
        make (u, characters_kw) (text "CHARACTERS");
        make (u, class_kw) (text "CLASS");
        make (u, close_kw) (text "CLOSE");
        make (u, code_set_kw) (text "CODE-SET");
        make (u, collating_kw) (text "COLLATING");
        make (u, comma_kw) (text "COMMA");
        make (u, common_kw) (text "COMMON");
        make (u, comp_kw) (text "COMP");
        make (u, comp_1_kw) (text "COMP-1");
        make (u, comp_2_kw) (text "COMP-2");
        make (u, comp_3_kw) (text "COMP-3");
        make (u, comp_4_kw) (text "COMP-4");
        make (u, comp_5_kw) (text "COMP-5");
        make (u, computational_kw) (text "COMPUTATIONAL");
        make (u, computational_1_kw) (text "COMPUTATIONAL-1");
        make (u, computational_2_kw) (text "COMPUTATIONAL-2");
        make (u, computational_3_kw) (text "COMPUTATIONAL-3");
        make (u, computational_4_kw) (text "COMPUTATIONAL-4");
        make (u, computational_5_kw) (text "COMPUTATIONAL-5");
        make (u, compute_kw) (text "COMPUTE");
        make (u, configuration_kw) (text "CONFIGURATION");
        make (u, contains_kw) (text "CONTAINS");
        make (u, content_kw) (text "CONTENT");
        make (u, continue_kw) (text "CONTINUE");
        make (u, converting_kw) (text "CONVERTING");
        make (u, copy_kw) (text "COPY");
        make (u, corr_kw) (text "CORR");
        make (u, corresponding_kw) (text "CORRESPONDING");
        make (u, count_kw) (text "COUNT");
        make (u, currency_kw) (text "CURRENCY");
        make (u, data_kw) (text "DATA");
        make (u, date_kw) (text "DATE");
        make (u, date_compiled_kw) (text "DATE-COMPILED");
        make (u, date_written_kw) (text "DATE-WRITTEN");
        make (u, day_kw) (text "DAY");
        make (u, day_of_week_kw) (text "DAY-OF-WEEK");
        make (u, dbcs_kw) (text "DBCS");
        make (u, debugging_kw) (text "DEBUGGING");
        make (u, debug_item_kw) (text "DEBUG-ITEM");
        make (u, decimal_point_kw) (text "DECIMAL-POINT");
        make (u, declaratives_kw) (text "DECLARATIVES");
        make (u, delete_kw) (text "DELETE");
        make (u, delimited_kw) (text "DELIMITED");
        make (u, delimiter_kw) (text "DELIMITER");
        make (u, depending_kw) (text "DEPENDING");
        make (u, descending_kw) (text "DESCENDING");
        make (u, display_kw) (text "DISPLAY");
        make (u, display_1_kw) (text "DISPLAY-1");
        make (u, divide_kw) (text "DIVIDE");
        make (u, division_kw) (text "DIVISION");
        make (u, down_kw) (text "DOWN");
        make (u, duplicates_kw) (text "DUPLICATES");
        make (u, dynamic_kw) (text "DYNAMIC");
        make (u, ebcdic_kw) (text "EBCDIC");
        make (u, egcs_kw) (text "EGCS");
        make (u, else_kw) (text "ELSE");
        make (u, end_kw) (text "END");
        make (u, end_add_kw) (text "END-ADD");
        make (u, end_call_kw) (text "END-CALL");
        make (u, end_compute_kw) (text "END-COMPUTE");
        make (u, end_delete_kw) (text "END-DELETE");
        make (u, end_divide_kw) (text "END-DIVIDE");
        make (u, end_evaluate_kw) (text "END-EVALUATE");
        make (u, end_if_kw) (text "END-IF");
        make (u, ending_kw) (text "ENDING");
        make (u, end_multiply_kw) (text "END-MULTIPLY");
        make (u, end_of_page_kw) (text "END-OF-PAGE");
        make (u, end_perform_kw) (text "END-PERFORM");
        make (u, end_read_kw) (text "END-READ");
        make (u, end_return_kw) (text "END-RETURN");
        make (u, end_rewrite_kw) (text "END-REWRITE");
        make (u, end_search_kw) (text "END-SEARCH");
        make (u, end_start_kw) (text "END-START");
        make (u, end_string_kw) (text "END-STRING");
        make (u, end_subtract_kw) (text "END-SUBTRACT");
        make (u, end_unstring_kw) (text "END-UNSTRING");
        make (u, end_write_kw) (text "END-WRITE");
        make (u, entry_kw) (text "ENTRY");
        make (u, environment_kw) (text "ENVIRONMENT");
        make (u, eop_kw) (text "EOP");
        make (u, equal_kw) (text "EQUAL");
        make (u, error_kw) (text "ERROR");
        make (u, evaluate_kw) (text "EVALUATE");
        make (u, every_kw) (text "EVERY");
        make (u, examine_kw) (text "EXAMINE");
        make (u, exception_kw) (text "EXCEPTION");
        make (u, exit_kw) (text "EXIT");
        make (u, extend_kw) (text "EXTEND");
        make (u, external_kw) (text "EXTERNAL");
        make (u, false_kw) (text "FALSE");
        make (u, fd_kw) (text "FD");
        make (u, file_kw) (text "FILE");
        make (u, file_control_kw) (text "FILE-CONTROL");
        make (u, filler_kw) (text "FILLER");
        make (u, first_kw) (text "FIRST");
        make (u, footing_kw) (text "FOOTING");
        make (u, for_kw) (text "FOR");
        make (u, from_kw) (text "FROM");
        make (u, giving_kw) (text "GIVING");
        make (u, global_kw) (text "GLOBAL");
        make (u, go_kw) (text "GO");
        make (u, goback_kw) (text "GOBACK");
        make (u, greater_kw) (text "GREATER");
        make (u, high_value_kw) (text "HIGH-VALUE");
        make (u, high_values_kw) (text "HIGH-VALUES");
        make (u, id_kw) (text "ID");
        make (u, identification_kw) (text "IDENTIFICATION");
        make (u, if_kw) (text "IF");
        make (u, in_kw) (text "IN");
        make (u, index_kw) (text "INDEX");
        make (u, indexed_kw) (text "INDEXED");
        make (u, initial_kw) (text "INITIAL");
        make (u, initialize_kw) (text "INITIALIZE");
        make (u, input_kw) (text "INPUT");
        make (u, input_output_kw) (text "INPUT-OUTPUT");
        make (u, inspect_kw) (text "INSPECT");
        make (u, installation_kw) (text "INSTALLATION");
        make (u, into_kw) (text "INTO");
        make (u, invalid_kw) (text "INVALID");
        make (u, i_o_kw) (text "I-O");
        make (u, i_o_control_kw) (text "I-O-CONTROL");
        make (u, is_kw) (text "IS");
        make (u, just_kw) (text "JUST");
        make (u, justified_kw) (text "JUSTIFIED");
        make (u, kanji_kw) (text "KANJI");
        make (u, key_kw) (text "KEY");
        make (u, label_kw) (text "LABEL");
        make (u, leading_kw) (text "LEADING");
        make (u, left_kw) (text "LEFT");
        make (u, length_kw) (text "LENGTH");
        make (u, less_kw) (text "LESS");
        make (u, linage_kw) (text "LINAGE");
        make (u, linage_counter_kw) (text "LINAGE-COUNTER");
        make (u, line_kw) (text "LINE");
        make (u, lines_kw) (text "LINES");
        make (u, linkage_kw) (text "LINKAGE");
        make (u, lock_kw) (text "LOCK");
        make (u, low_value_kw) (text "LOW-VALUE");
        make (u, low_values_kw) (text "LOW-VALUES");
        make (u, memory_kw) (text "MEMORY");
        make (u, merge_kw) (text "MERGE");
        make (u, mode_kw) (text "MODE");
        make (u, modules_kw) (text "MODULES");
        make (u, more_labels_kw) (text "MORE-LABELS");
        make (u, move_kw) (text "MOVE");
        make (u, multiple_kw) (text "MULTIPLE");
        make (u, multiply_kw) (text "MULTIPLY");
        make (u, native_kw) (text "NATIVE");
        make (u, negative_kw) (text "NEGATIVE");
        make (u, next_kw) (text "NEXT");
        make (u, no_kw) (text "NO");
        make (u, not_kw) (text "NOT");
        make (u, null_kw) (text "NULL");
        make (u, nulls_kw) (text "NULLS");
        make (u, numeric_kw) (text "NUMERIC");
        make (u, numeric_edited_kw) (text "NUMERIC-EDITED");
        make (u, object_computer_kw) (text "OBJECT-COMPUTER");
        make (u, occurs_kw) (text "OCCURS");
        make (u, of_kw) (text "OF");
        make (u, off_kw) (text "OFF");
        make (u, omitted_kw) (text "OMITTED");
        make (u, on_kw) (text "ON");
        make (u, open_kw) (text "OPEN");
        make (u, optional_kw) (text "OPTIONAL");
        make (u, or_kw) (text "OR");
        make (u, order_kw) (text "ORDER");
        make (u, organization_kw) (text "ORGANIZATION");
        make (u, other_kw) (text "OTHER");
        make (u, output_kw) (text "OUTPUT");
        make (u, overflow_kw) (text "OVERFLOW");
        make (u, packed_decimal_kw) (text "PACKED-DECIMAL");
        make (u, padding_kw) (text "PADDING");
        make (u, page_kw) (text "PAGE");
        make (u, password_kw) (text "PASSWORD");
        make (u, perform_kw) (text "PERFORM");
        make (u, pic_kw) (text "PIC");
        make (u, picture_kw) (text "PICTURE");
        make (u, pointer_kw) (text "POINTER");
        make (u, position_kw) (text "POSITION");
        make (u, positive_kw) (text "POSITIVE");
        make (u, procedure_kw) (text "PROCEDURE");
        make (u, procedures_kw) (text "PROCEDURES");
        make (u, proceed_kw) (text "PROCEED");
        make (u, program_kw) (text "PROGRAM");
        make (u, program_id_kw) (text "PROGRAM-ID");
        make (u, quote_kw) (text "QUOTE");
        make (u, quotes_kw) (text "QUOTES");
        make (u, random_kw) (text "RANDOM");
        make (u, read_kw) (text "READ");
        make (u, record_kw) (text "RECORD");
        make (u, recording_kw) (text "RECORDING");
        make (u, records_kw) (text "RECORDS");
        make (u, redefines_kw) (text "REDEFINES");
        make (u, reel_kw) (text "REEL");
        make (u, reference_kw) (text "REFERENCE");
        make (u, relative_kw) (text "RELATIVE");
        make (u, release_kw) (text "RELEASE");
        make (u, remainder_kw) (text "REMAINDER");
        make (u, remarks_kw) (text "REMARKS");
        make (u, removal_kw) (text "REMOVAL");
        make (u, renames_kw) (text "RENAMES");
        make (u, replacing_kw) (text "REPLACING");
        make (u, rerun_kw) (text "RERUN");
        make (u, reserve_kw) (text "RESERVE");
        make (u, return_kw) (text "RETURN");
        make (u, return_code_kw) (text "RETURN-CODE");
        make (u, reversed_kw) (text "REVERSED");
        make (u, rewind_kw) (text "REWIND");
        make (u, rewrite_kw) (text "REWRITE");
        make (u, right_kw) (text "RIGHT");
        make (u, rounded_kw) (text "ROUNDED");
        make (u, run_kw) (text "RUN");
        make (u, same_kw) (text "SAME");
        make (u, sd_kw) (text "SD");
        make (u, search_kw) (text "SEARCH");
        make (u, section_kw) (text "SECTION");
        make (u, security_kw) (text "SECURITY");
        make (u, segment_limit_kw) (text "SEGMENT-LIMIT");
        make (u, select_kw) (text "SELECT");
        make (u, sentence_kw) (text "SENTENCE");
        make (u, separate_kw) (text "SEPARATE");
        make (u, sequence_kw) (text "SEQUENCE");
        make (u, sequential_kw) (text "SEQUENTIAL");
        make (u, set_kw) (text "SET");
        make (u, shift_in_kw) (text "SHIFT-IN");
        make (u, shift_out_kw) (text "SHIFT-OUT");
        make (u, sign_kw) (text "SIGN");
        make (u, size_kw) (text "SIZE");
        make (u, sort_kw) (text "SORT");
        make (u, sort_control_kw) (text "SORT-CONTROL");
        make (u, sort_core_size_kw) (text "SORT-CORE-SIZE");
        make (u, sort_file_size_kw) (text "SORT-FILE-SIZE");
        make (u, sort_merge_kw) (text "SORT-MERGE");
        make (u, sort_message_kw) (text "SORT-MESSAGE");
        make (u, sort_mode_size_kw) (text "SORT-MODE-SIZE");
        make (u, sort_return_kw) (text "SORT-RETURN");
        make (u, source_computer_kw) (text "SOURCE-COMPUTER");
        make (u, space_kw) (text "SPACE");
        make (u, spaces_kw) (text "SPACES");
        make (u, special_names_kw) (text "SPECIAL-NAMES");
        make (u, standard_kw) (text "STANDARD");
        make (u, standard_1_kw) (text "STANDARD-1");
        make (u, standard_2_kw) (text "STANDARD-2");
        make (u, start_kw) (text "START");
        make (u, status_kw) (text "STATUS");
        make (u, stop_kw) (text "STOP");
        make (u, string_kw) (text "STRING");
        make (u, subtract_kw) (text "SUBTRACT");
        make (u, suppress_kw) (text "SUPPRESS");
        make (u, symbolic_kw) (text "SYMBOLIC");
        make (u, sync_kw) (text "SYNC");
        make (u, synchronized_kw) (text "SYNCHRONIZED");
        make (u, tally_kw) (text "TALLY");
        make (u, tallying_kw) (text "TALLYING");
        make (u, tape_kw) (text "TAPE");
        make (u, test_kw) (text "TEST");
        make (u, than_kw) (text "THAN");
        make (u, then_kw) (text "THEN");
        make (u, through_kw) (text "THROUGH");
        make (u, thru_kw) (text "THRU");
        make (u, time_kw) (text "TIME");
        make (u, times_kw) (text "TIMES");
        make (u, to_kw) (text "TO");
        make (u, top_kw) (text "TOP");
        make (u, trailing_kw) (text "TRAILING");
        make (u, true_kw) (text "TRUE");
        make (u, unit_kw) (text "UNIT");
        make (u, unstring_kw) (text "UNSTRING");
        make (u, until_kw) (text "UNTIL");
        make (u, up_kw) (text "UP");
        make (u, upon_kw) (text "UPON");
        make (u, usage_kw) (text "USAGE");
        make (u, use_kw) (text "USE");
        make (u, using_kw) (text "USING");
        make (u, value_kw) (text "VALUE");
        make (u, values_kw) (text "VALUES");
        make (u, varying_kw) (text "VARYING");
        make (u, when_kw) (text "WHEN");
        make (u, when_compiled_kw) (text "WHEN-COMPILED");
        make (u, with_kw) (text "WITH");
        make (u, words_kw) (text "WORDS");
        make (u, working_storage_kw) (text "WORKING-STORAGE");
        make (u, write_kw) (text "WRITE");
        make (u, write_only_kw) (text "WRITE-ONLY");
        make (u, zero_kw) (text "ZERO");
        make (u, zeroes_kw) (text "ZEROES");
        make (u, zeros_kw) (text "ZEROS");

        make (u, ws) R.(star (codes " \n\r\t"));
        make (u, e) eof;
      ]
  end)

module B = Benchmark.Make(X)

let _ =
  (*let d = X.driver (X.tables ()) in
  let t = Sys.time() in
  X.Run.file (fun c -> d#read c) "linear/sample2002.cbl";
  Fmt.pr "%b %f@." d#accept (Sys.time() -. t);
  (*Fmt.pr "@[%s@]" (Dot.string_of_graph d#to_dot);*)
  (*Fmt.pr "@[%s@]" (Dot.string_of_graph (T.Node_packed_forest.to_dot d#forest))*)
  Fmt.pr "@[%a@]" Trace.pp d#trace;*)

  B.benchmark ()

#!/bin/sh
echo "Making backup..."
cp $1 $1.bkup
echo "Applying regexes..."
cat $1 | sed -E 's/erast2esast\(/erast_to_esast\(/g; s/parseModule\(/parse_module\(/g; s/parseFunctions\(/parse_functions\(/g; s/parseFunction\(/parse_function\(/g; s/encapsulateExpressions\(/encapsulate_expressions\(/g; s/listCheck\(/list_check\(/g; s/parseNode\(/parse_node\(/g; s/parseCall\(/parse_call\(/g; s/parseValues\(/parse_values\(/g; s/parseVar\(/parse_var\(/g; s/parseSeq\(/parse_seq\(/g; s/parseLet\(/parse_let\(/g; s/parseApply\(/parse_apply\(/g; s/parseLiteral\(/parse_literal\(/g; s/parseTuple\(/parse_tuple\(/g; s/parseCons\(/parse_cons\(/g; s/parseTry\(/parse_try\(/g; s/parsePrimop\(/parse_primop\(/g; s/parseLetrec\(/parse_letrec\(/g; s/parseCase\(/parse_case\(/g; s/parseFunctionCase\(/parse_function_case\(/g; s/parseConsChain\(/parse_cons_chain\(/g; s/parseCaseClauses\(/parse_case_clauses\(/g; s/assembleCaseCondition\(/assemble_case_condition\(/g; s/recurseVarDeclaration\(/recurse_var_declaration\(/g; s/recurseVarAssignments\(/recurse_var_assignments\(/g; s/assembleMatchCalls\(/assemble_match_calls\(/g; s/assignMatchedVars\(/assign_matched_vars\(/g; s/assembleSequence\(/assemble_sequence\(/g; s/tupleListToIdentifierList\(/tuple_list_to_identifier_list\(/g; s/tupleList_getVars_3\(/tuple_list_get_vars_3\(/g; s/tupleList_getVars_2\(/tuple_list_get_vars_2\(/g; s/tupleList_getVars_1\(/tuple_list_get_vars_1\(/g; s/declaratorsFromList\(/declarators_from_list\(/g; s/tup2list\(/tup_to_list\(/g' >> $1.regexed
rm $1
mv $1.regexed $1
echo "ok."

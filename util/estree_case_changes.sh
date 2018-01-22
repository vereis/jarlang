#!/bin/sh
echo "Making backup..."
cp $1 $1.bkup
echo "Applying regexes..."
cat $1 | sed -E 's/updateRecord\(/update_record\(/g; s/sourceLocation\(/source_location\(/g; s/addLocationData\(/add_location_data\(/g; s/spreadElement\(/spread_element\(/g; s/useStrict\(/use_strict\(/g; s/catchClause\(/catch_clause\(/g; s/switch([A-Za-z]+)\(/switch_\L\1\(/g; s/for([A-Za-z]+)Statement\(/for_\L\1_statement\(/g; s/([A-Za-z]+)Statement\(/\L\1_statement\(/g; s/([A-Za-z]+)Declaration\(/\1_declaration\(/g; s/([A-Za-z]+)Declarator\(/\1_declarator\(/g; s/([A-Za-z]+)Expression\(/\L\1_expression\(/g;' >> $1.regexed
rm $1
mv $1.regexed $1
echo "ok."

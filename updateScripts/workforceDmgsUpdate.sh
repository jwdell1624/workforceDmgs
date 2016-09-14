#!/bin/bash
set -eou pipefail

####################################################################################################

# This script updates data for Workforce Demographics Dashboard

####################################################################################################

# Find the date of the most recent data
fileDate=$(jdwh -c "select max(snpsht_dt) from epgnrldmv.fact_sap_snpsht_dmgs" | grep -Eo [0-9]{4}-[0-9]{2}-[0-9]{2})
newDate=$(date --date="$fileDate" +'%d.%m.%Y')
echo "Data is as at $newDate" >| "$workforceDmgsData/dateComment.txt"

# Extract latest data 
Rscript workforceDmgsUpdate.R >| "$workforceDmgsData/wddDmgs.csv"

#________________________________________________________________________________________

echo $'\n'DEMOGRAPHICS DATA UPDATE COMPLETE
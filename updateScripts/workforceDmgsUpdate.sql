SELECT
a.Snpsht_dt                         AS Snapshot_Date
, a.Posn_Locn_Cd                    AS Position_Location
, a.Tax_Ofc_Subpln_Cd               AS Subplan
, a.Tax_Ofc_Org_Unt_SAP_BSL_Cd      AS BSL
, a.Org_Cst_Cntr_Cd                 AS Cost_Centre_Code
, a.Org_Cst_Cntr_Txt                AS Cost_Centre_Text
, a.Tax_Ofc_Org_Unt_Id              AS Org_Unit_ID
, b.Tax_Ofc_Org_Unt_Brnch           AS Org_Unit_Branch
, b.Level_4                         AS Org_Unit_Team
, CASE
    WHEN a.Pblc_Srvc_Clsn_Cd = 'SES3' THEN 'SES2'
  ELSE a.Pblc_Srvc_Clsn_Cd
END                                 AS Actual_Classification
, a.Job_Fmly_Txt                    AS Job_Family
, a.Tax_Ofc_Prsn_Hd_Cnt             AS HC
, a.Full_Tm_Eqvlnt_Pct              AS HR_FTE
, a.Ag_Rng_Shrt_At_Extrct_Txt       AS Age_Range_5yr
, a.Ag_Rng_Lng_At_Extrct_Txt        AS Age_Range_10yr
, a.Tax_Ofc_Prsn_Ag_At_Extrct_Num   AS Age_Integer
, a.TaxOfcPrsnDcmlAgAtExtrctNum     AS Age_Decimal
, a.APS_Tnr_Rng_At_Extrct_Txt       AS APS_Tenure_Range
, a.APS_Tnr_in_Yrs_At_Extrct_Cnt    AS APS_Tenure
, a.ATO_Tnr_Rng_At_Extrct_Txt       AS ATO_Tenure_Range
, a.ATO_Tnr_in_Yrs_At_Extrct_Cnt    AS ATO_Tenure
, a.Emple_Grp_Prmncy_Sts_Nm         AS Perm_Temp
, a.Emple_Grp_FTPT_Sts_Nm           AS Full_Part
, a.Cntrct_End_Dt                   AS Contract_End_Date
, a.Sex_Dcd                         AS Gender
, a.Tax_Ofc_Prsn_Temp_Prfmc_Ind     AS TP_Indicator
, c.eed_nesb1_cnt                   AS NESB1_HC
, c.eed_nesb2_cnt                   AS NESB2_HC
, c.eed_indgns_cnt                  AS Indigenous_HC
, c.eed_dsblty_cnt                  AS Disability_HC
, c.Mblty_Applcntn_Ind              AS Mobility_Indicator
, c.eRecruit_OOM_Ind                AS OOM_Indicator
, c.eRecruit_OOM_Cnt                AS OOM_Count
, c.MDP_Crs_Prtcpn_Txt              AS MDP_Status
, c.Crs_Cmpltn_RATE                 AS MDP_Completion_Percent
, c.R12_F2F_Nbr_Int                 AS F2F_Count
, c.R12_eLRN_Nbr                    AS eLRN_Count
, c.R12_Extrnl_Num                  AS External_Count
, c.R12_Extrnl_Cst                  AS External_Cost
, c.Mgr_Ind                         AS Manager_Indicator
, c.Wkfc_Segment                    AS Work_Segment
, c.Wkfce_Function                  AS Work_Function
, c.Wkfc_Comms_Persona              AS Comms_Persona
FROM epgnrldmv.fact_sap_snpsht_dmgs AS a
LEFT JOIN eadppert.dim_branch       AS b
ON (a.Snpsht_dt = b.Snpsht_dt AND a.Tax_Ofc_Org_Unt_Id = b.Tax_Ofc_Org_Unt_Id)
LEFT JOIN eadppert.ubnkp_ssot_mega  AS c
ON (a.Snpsht_dt = c.Snpsht_dt AND a.Tax_Ofc_Prsn_Pers_Num = c.Tax_Ofc_Prsn_Pers_Num)
WHERE a.Pblc_Srvc_Clsn_Cd NOT IN ('COMM', 'COM2')
AND a.Snpsht_dt = (SELECT MAX(Snpsht_dt) FROM epgnrldmv.fact_sap_snpsht_dmgs)
;
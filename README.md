# PSPS Data from California

Public safety power shutoffs (PSPS) events are planned power outages. PSPS events are a measure taken by governments and electric utility companies in an effort to protect public safety. Their goal is to prevent wildfires that may stem from down electrical wires during severe weather.

These data from PSE compile PSPS event reports from (October 2013 through December 2022). The raw data are unique on (1) PSPS event ID, and (2) Circuit. Each PSPS event may affect multiple circuits, so there is a row of data for each combination. Of note, the PSPS sub event ID represents a concatination of the PSPS event ID and the circuit name and can be used as a unique identifier. The raw have the following columns: 
- PSPS_Event_ID
- Sub_Event_ID
- Utility
- Circuit_Name_ICA
- Circuit_Name_Reported
- Outage_Start_Year
- Outage_Start
- Outage_Full_Restoration
- Outage_Duration
- Duration_Days
- Duration_Hours
- Duration_Minutes
- Customer_Minutes
- Total_Customers_Impacted
- Residential_Customers_Impacted
- Commercial_Industrial_Customers_Impacted
- Medical_Baseline_Customers_Impacted
- Other_Customers_Impacted
- Notes_Reported
- PSE_Notes

The variables we are using are explained below:
- PSPS_Event_ID: Unique identifier for each PSPS event (PRIMARY KEY)
- Sub_Event_ID: Concatination of PSPS_Event_ID and Circuit_Name_ICA (PRIMARY KEY)
- Circuit_Name_ICA: Name of the circuit affected. Of note, we do not know if the full circuit was without power, we simply know that at least part of it was. Circuits can be de-energized and re-energized throughout an event, so they also may not have been without power for the entire duration of the event. (PRIMARY KEY)
- Outage_Start: The outage start corresponds to the time when the first customer lost power during a given event.
- Outage_Full_Restoration: The outage end corresponds to the time when the last customer's power was restored during a given event. 
- Outage_Duration: The duration is the time difference between outage start and end, which means it is the total time that at least 1 customer was without power during a given event. 
- Total_Customers_Impacted: Number of customers impacted in a given row (so on a given circuit for a given event). There is no information about where on a circuit these customers are, this is a simple count. 
- Residential_Customers_Impacted: Number of residential customers impacted in a given row (so on a given circuit for a given event). There is no information about where on a circuit these customers are, this is a simple count. 
- Commercial_Industrial_Customers_Impacted: Number of commercial customers impacted in a given row (so on a given circuit for a given event). There is no information about where on a circuit these customers are, this is a simple count. 
- Medical_Baseline_Customers_Impacted: Number of customers with specific medical dependencies impacted in a given row (so on a given circuit for a given event). There is no information about where on a circuit these customers are, this is a simple count. Medical Baseline customers are those signed up for their utility's medical baseline program. These customers are typically given more advance (and sometimes more intensive) notice ahead of outages. They may also be eligible to apply for other resilience services. Program link: https://urldefense.com/v3/__https:/www.pge.com/en/account/billing-and-assistance/financial-assistance/medical-baseline-program.html__;!!K-Hz7m0Vt54!kLlTFQ5dRg-IX3V1Hl4Z_ygZBdq4TGQWpolaA30kHn-zb4oVztyUtTEREl4ow246di6-CnT3XQsD8RCTA2mbzw$
- Other_Customers_Impacted: Number of other types of customers impacted in a given row (so on a given circuit for a given event). There is no information about where on a circuit these customers are, this is a simple count. 


In addition to these data, we have polyline and polygon files for SGE, PGE, and SDGE circuits.

We process the data according to the following steps: 
1. Clean up variable names and classes.
2. Make each row a circuit-event-hr (ie sub_event-hr)
    - if there is overlap on times/circuits, average the rows
    - calculate the number of customers impacted in each circuit hour
    - calculate the number of customers hours without power in each sub_event (absolute metric of power loss)
2. Map data to either census tracts or zctas (IN PYTHON): 
    - map circuits to pixels using CA gridded pop data
    - map pixels to zctas
3. Incorporate a washout period if relevant. Currently doing this by: 
    - anything within a week prior to an event gets collapsed to one event. 
    - anything >1 and <4 weeks prior gets excluded.
4. Merge on wildfire data and create a binary indicator 
5. Expand out to hourly dataset (though we don't have hourly granularity, so we may consider skipping this as the metrics are all constant across all hours of an event.)
6. Classify events as mild/moderate/severe


The output data contain the following columns: 
- geoid OR zcta: Either the census tract geoid or the zcta for a given row, depending on the resolution of the output dataset. Both are available. 
- psps_event_id: Same as above -- event ID of a given PSPS event.
- total_customers_impacted: Total customers impacted in a given geoid or zcta and for a given PSPS event. 
- outage_start: Same as above -- the start time for the outage, which represents the time when the first customer lost power.
- outage_end: Same as above -- the end time for the outage, which represents the time when the last customer had their power restored.
- duration: Same as above -- the difference between the outage start and end time. 
- pop: Population for a given geoid or zcta, derived from gridded population data in CA (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0270746)
- pct_cust_out: Total customers impacted divided by population to get a proportion of customers impacted.
- wf_exposed: Binary indicator of whether there was wildfire smoke on the first day of a PSPS outage. (NOTE: we may want to redo this to use different lags of wf smoke. Given that the rows are not hour/day specific, we merged on this exposure variable to the start day of the event with a lag of 0.)
- hybrid: This hybrid metric is a measure of severity of an outage and is derived by multiplying the total number of customers out by the proportion of customers out. The goal is to compile a metric that incorporates both the absolute and relative severity of the event. 
- severity_customers: This categorical indicator categorizes a PSPS event as mild, moderate, or severe based on the number of customers impacted. We do this by using values near the tertiles of the number of customers impacted to create cutoffs for these three categories. 
- severity_hybrid: This categorical indicator categorizes a PSPS event as mild, moderate, or severe based on the hybrid metric described above. We do this by using values near the tertiles of the hybrid metric to create cutoffs for these three categories. 

Pending tasks: 
1. For the wildfire smoke exposure, we want to switch to take pop weighted mean rather than just a mean. This means that if there are many tracts per zip, we weight by the percent overlap of each tract with each zip. Additionally, we can consider incorporating wildfire exposure at different lags and/or throughout the PSPS event. 
2. We may consider switching from using total customers impacted to residential customers impacted. 


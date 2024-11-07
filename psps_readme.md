
THIS README CAME FROM THE DATA PROVIDER


README  | 2023.07.psps_outages_by_census_tract.csv
--------------------------------------------------------------------------------
Dataset Overview
--------------------------------------------------------------------------------
Filename: 2023.07.psps_outages_by_census_tract.csv
Supplementary files:
2023.07.psps_no_geospatial_data.csv
2023.07.psps_no_customer_impacts.csv
2023.07.clean_psps_data.csv
--------------------------------------------------------------------------------
Data Sources
(1) CPUC PSPS Event Rollup (October 2013 through December 2022).* (Dataset & Info)
(2) PSPS post event reports from various California IOUs.* Electronic data requests from PG&E.**
(3) Circuit locations from Interconnection Capacity Analysis data via PG&E/SCE/SDG&E.***
(4) 2010 census tract boundaries and Place boundaries from US Census Bureau.***
(5) Population data from CalEnviroScreen4.0 via OEHHA.***
* Most recent download: June 2023.
 ** Most recent download: October 2022
***  Most recent download: September 2022
--------------------------------------------------------------------------------
Status
This is a living dataset, as new PSPS events can / will be added after they occur. (It may also be modified to refine the assignment of  impacted customers to census tracts.)
--------------------------------------------------------------------------------
Caveats, limitations, and quirks
(1) In some cases, households experiencing PSPS impacts are assigned to the incorrect census tract or missing altogether due to a lack of information on some circuit locations, the location of specific circuit segments, and the locations of households served by circuits that border multiple tracts.
(1.1) Impacted customers are assumed to be evenly distributed along circuits, which does not accurately reflect the distribution of customers experiencing outages, particularly for more recent events. (For specific census tract impacts from PSPS events for events from mid-2021 onwards, see each utility’s PSPS post event report attachments.)
(2) Only includes data from California’s investor-owned utilities (PG&E, SCE, SDG&E, PacifiCorp, Liberty, BVES), as other utilities have different reporting requirements for outages.
(3) This dataset is consistent with first-filed PSPS post event reports. Where utilities amended their initial reports, updates are not reflected in the data. One exception is PG&E’s PSPS event starting 2019.10.09. (A check of pre- and post-amended data for this event revealed relatively small shifts in overall impacts.)
(4) Outliers: A few sub events are listed as having very long (> month) outage durations. This reflects instances where utility infrastructure was damaged while the lines were de-energized and restoration was slow. (These longer full restoration dates/times are often not included in the CPUC rollup, as the circuits were not yet restored when the initial PSPS post event report was published.)
--------------------------------------------------------------------------------
Dataset Notes
--------------------------------------------------------------------------------
(1) The underlying PSPS data is the CPUC rollup modified based on cross checks with specific post event reports, data requests to PG&E, and the amended PG&E post-event report for 10/09/2019. It has 5,351 rows, with each row representing either a circuit or a portion of a circuit that was de-energized during a PSPS event.
(1.1) This does not include rows that were removed from the CPUC rollup during data cleaning. These 33 rows were removed because they were labeled ‘permanently de-energized,’ ‘idle line,’ or were missing outage start or restoration data and lacking customer impacts.
(1.2) Data cleaning included identifying and removing the above rows, ensuring formatting and data type consistency, spot checking against post event reports, and matching circuit name formats for each utility to the naming conventions in each of their geospatial circuit data files.

(2) Of 5,351 rows remaining after cleaning, 4,778 rows (equaling 3,639 sub events) had customer impacts.
(2.1) As PSPS events include numerous circuits de-energized and re-energized over various time spans and locations, sub events were used to classify a single circuit de-energized and restored as part of a wider PSPS event. The sub event ID combines the utility name, the PSPS event start date, the circuit name, and a number indicating whether this was the first, second, or third on/off for this circuit during this event. (In some cases, the same circuit is de-energized and restored multiple times during the same PSPS event. When this is indicated within the CPUC rollup, each outage/restoration cycle is given its own sub-event ID, as customers are experiencing a new outage even if it’s occurring during the same general PSPS event.)
(2.2) The CPUC rollup sometimes lists the same circuit in multiple rows with different numbers of impacted customers and outage durations (denoting a circuit that was segmented and either de-energized or restored piecemeal). We don’t have geospatial data on circuit segments–only the full circuit–so data was grouped by sub event ID before being matched to circuit locations. Durations were customer-weighted as part of this consolidation to the full circuit.
(2.3) Geospatial circuit data from PG&E, SCE, and SDG&E was intersected with polygons of California census tracts (using 2010 boundaries) to indicate what percentage of each circuit exists within different tracts. As PacifiCorp does not publish geospatial circuit data, a shapefile of PacifiCorp circuits was created based on their post-event reports and geographic data from the US Census Bureau, representing the approximate locations and percentages of PSPS-impacted circuits in different census tracts in PacifiCorp’s service territory.
(2.4) Sub events were then matched to a csv of circuit/census tract intersections.
(2.5) 98.5% of the sub events (99% of outage customer hours) have geospatial circuit matches.
(2.5.1) There are 3,399 sub event IDs (1,154 unique circuits) with geospatial matches.
(2.5.2) There are 240 sub event IDs (98 unique circuits) that have no geospatial matches.

(3) There were also 573 rows with no customer impacts.
(3.1) These represent circuits that were de-energized but whose customers nonetheless retained power. It’s often transmission or sub transmission circuits. 
(3.2) Of these, 334 sub events (155 unique circuits) have geospatial matches.
(3.3) Of these, 131 (86 unique circuits) have no geospatial matches.

(4) Further methodology & data sources (used for the public safety power shutoff maps)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

DATA DICTIONARY | 2023.07.psps_outages_by_census_tract.csv
--------------------------------------------------------------------------------
Data Dictionary
--------------------------------------------------------------------------------
Summary: Each row represents a circuit, or a portion of a circuit, that was de-energized during a PSPS event within a specific census tract, along with the estimated number of customers impacted within that census tract.

Contact info: Contact Bethany Kwoka (bkwoka@psehealthyenergy.org) with any questions.

Date of most recent update: June 2023
--------------------------------------------------------------------------------
Variable Descriptions
--------------------------------------------------------------------------------
Variable Name:  Census Tract GeoID
    ID: GEOID
    Description: Census tract unique identifier.
    Format: string
    Example: 6013356002
    Units: NA
--------------------------------------------------------------------------------
Variable Name:  PSPS Event ID
    ID: PSPS_Event_ID
    Description: Identifier for full PSPS events, which generally include multiple circuits. A concatenation of the reporting utility and the PSPS event start date.
    Format: string
    Example: PG&E_2019/10/26
    Units: NA
--------------------------------------------------------------------------------
Variable Name:  Sub Event ID
    ID: Sub_Event_ID
    Description: Identifier for a PSPS outage on a particular circuit. A concatenation of the PSPS Event ID, circuit name as it appears in the geospatial data, and outage number on the circuit within that event.
    Format: string
    Example: PG&E_2019/10/26_ALHAMBRA 1105_1
    Units: NA
--------------------------------------------------------------------------------
Variable Name: Circuit Name (as it appears in the geospatial data)
    ID: Circuit_Name_ICA
    Description: Circuit name as it appears in either the ICA or other geospatial data.
    Format: string
    Example: ALHAMBRA 1105
    Units: NA
--------------------------------------------------------------------------------
Variable Name: Outage Start
    ID: Earliest_Outage_Start
    Description: The earliest date and time a customer was de-energization on this circuit during this PSPS outage. (Earliest accounts for different segments of a circuit being de-energized at different times.)
    Format: datetime object
    Example: 10/26/2019 20:19
    Units: NA
--------------------------------------------------------------------------------
Variable Name: Outage Restoration
    ID: Lastest_Outage_Restoration
    Description: Date and time when the last customers on a circuit had power restored during this outage on this circuit.
    Format: datetime object
    Example: 10/28/2019 20:44
    Units: NA
--------------------------------------------------------------------------------
Variable Name: Outage Duration (in hours)
    ID: Customer_Weighted_Duration_Hrs
    Description: The average duration of the outage, weighted by the number of customers impacted. E.g., the total ‘outage customer hours’ (impacted customers multiplied by their outage durations) divided by the total number of customers impacted on that circuit. Note: this is specific to circuits, not tracts.
    Format: float64
    Example: 48.417
    Units: hours
--------------------------------------------------------------------------------
Variable Name: Customers Impacted 
    ID: Customers_Impacted
    Description: Number of customer accounts (residential, commercial, other) impacted by the PSPS outage on this circuit in this census tract.
    Format: float64
    Example: 1251.246
    Units: NA
--------------------------------------------------------------------------------
Variable Name: Census Tract Population 
    ID: Total_Population_CES
    Description: Population of the census tract (via CalEnviroScreen4.0 data).
    Format: int64
    Example: 5522
    Units: NA
--------------------------------------------------------------------------------
Variable Name:  Circuit/Census Tract Intersection (percent)
    ID: circuit_intersect_pct
    Description: The percentage (as a decimal) of the circuit that is within that census tract. 
    Format: float64
    Example: 0.731
    Units: NA
--------------------------------------------------------------------------------
Variable Name: Utility 
    ID: Utility
    Description: Utility reporting the PSPS outage
    Format: string
    Example: PG&E
    Units: NA
--------------------------------------------------------------------------------
Variable Name:  Outage Start (Year)
    ID: Outage_Start_Year
    Description: The year the PSPS event started
    Format: int64
    Example: 2019
    Units: NA
--------------------------------------------------------------------------------
Variable Name: Circuit Name (as reported in the CPUC rollup or PSPS post-event report)
    ID: Circuit_Name_PSPS_Reported
    Description: Circuit name as reported in the CPUC rollup or a PSPS post-event report
    Format: List of strings
    Example: ['ALHAMBRA-1105*']
    Units: NA



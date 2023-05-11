set maxvar 10000
clear mata

use "C:\Users\Rajsi Sah\Desktop\NFHS-5\Individual Recode\IAIR7DFL.DTA", clear

*****keeping only selected variables*******

keep caseid v000 v001 v002 v003 v004 v005 v007 v021 v022 v023 v024 v025 v026 v034 v040/*
*/ v042 v101 v102 v104 v106 v107 v113 v115 v116 v130 v131 v136 v137 v138 v139 v140 v141/*
*/ v149 v151 v152 awfactt awfactu awfactr awfacte awfactw v155 v157 v158 v159 v170 bord_01/*
*/ m2a_1 m2b_1 m2g_1 m2h_1 m2i_1 m2j_1 m2k_1 m2n_1 m3a_1 m3b_1/*
*/  m3c_1 m3g_1 m3h_1 m3k_1 m3n_1 m10_1 m14_1/*
*/  m15_1 m17_1 m18_1 m19a_1 m19_1 m57a_1 m57b_1 m57c_1 m57d_1 m57e_1 m57f_1 m57g_1 m57h_1/*
*/  m57i_1 m57j_1 m57k_1 m57l_1 m57m_1 m57n_1 m57o_1 m57s_1 m57t_1 m61_1 m65a_1 m65b_1 m65c_1/*
*/  m65d_1 m65e_1 m65f_1 m65g_1 m65h_1 m65i_1 m66_1 m70_1 m71_1 v481 v481a v481b v481c v481d v481e/*
*/  v481f v481g v481h v481x h1_1 hw2_1 hw3_1 hw5_1 hw8_1 hw11_1 v701 v702 v704 v705 v714 v715/*
*/  v716 v717 v743a v743b v743c v743d v743e v743f s413_1 s414_1 s451_1 s452a_1 s452b_1 s452c_1 s452d_1 s470_1/*
*/ v190 v191 v190a v191a s234 m1_1 m1d_1 s370c s370e s454_1 m13_1 m45_1 m46_1 sdist v731 v732 v739 v501
***** renaming variables *****

rename v024 state
rename v025 type_of_residence
rename v106 ind_high_education
rename v130 religion
rename v131 ethnicity
rename v136 hh_size
rename v151 hh_head_sex
rename v152 hh_head_age
rename awfactt awfactt
rename v155 literacy
rename v157 freq_newspaper
rename v158 freq_radio
rename v159 freq_tv
rename v170 bank_account
rename v190 wealth_index_tot
rename v191 wealth_score_tot
rename v190a wealth_index_ur
rename v191a wealth_score_ur
rename bord_01 birth_order
rename m2a_1 prenatal_doc
rename m2b_1 prenatal_midw
rename m2g_1 prenatal_dai
rename m2h_1 prenatal_comm
rename m2i_1 prenatal_icds
rename m2j_1 prenatal_asha
rename m2k_1 prenatal_other
rename m2n_1 prenatal_none
rename m3a_1 assist_doc
rename m3b_1 assist_midw
rename m3c_1 assist_other_hp
rename m3g_1 assist_dai
rename m3h_1 assist_friend
rename m3k_1 assist_other
rename m3n_1 assist_none
rename m10_1 desired_preg
rename m14_1 no_antenal_visits
rename m15_1 place_of_delivery
rename m17_1 caesarean
rename m18_1 size_at_birth
rename hw2_1 birth_wt_kg
rename hw3_1 birth_ht_cm
rename hw11_1 height_weight_dev
rename v701 partner_educ
rename v704 partner_occupation
rename v714 respondent_empl
rename v715 total_educ
rename v716 resp_occup
rename v743a decision_health
rename v743b decision_hhexp
rename v743c decision_dailyexp
rename v743d decison_visits
rename v743e decision_meals
rename v743f decision_money
rename s451_1 oop_transp
rename s452a_1 oop_hosp_stay
rename s452b_1 oop_hosp_tests
rename s452c_1 oop_hosp_meds
rename s452d_1 oop_other
rename s470_1 postnatal_check
rename s234 preg_completed
rename v481 health_insur
rename v481a health_insur_esis
rename v481b health_insur_cghs
rename v481c health_insur_state
rename v481d health_insur_rsby
rename v481e health_insur_chip
rename v481f health_insur_empl1
rename v481g health_insur_empl2
rename v481h health_insur_pvt
rename v481x health_insur_other
rename m1_1 tetanus_number
rename v137 nn_hh_children
rename v138 nn_hh_women
rename m1d_1 tetanus_last_year
rename m13_1 time_first_anc
rename m45_1 preg_iron_tab
rename m46_1 preg_iron_days
rename s454_1 total_cost_oop
rename m66_1 health_check_postdel
rename m70_1 baby_pnc
rename m71_1 pnc_time_after



****drop never pregnant women****
count if birth_order == . //230096
count if birth_order != . //494019
keep if birth_order != .  //230096 deleted

*** drop incomplete preg*****
gen preg_full = 0 if preg_completed == 1|preg_completed == 2|preg_completed == 3
replace preg_full = 1 if preg_completed == .
tab preg_full
keep if preg_full == 1 //24847 deleted

*** generate outcome variables interested in***
** indicator 1: ANTENATAL CARE **

*number of ANC visits*
br no_antenal_visits // the missing entries are the one with zero children in the hh
tab nn_hh_children
keep if nn_hh_children != 0 
count if no_antenal_visits == . //48,629 missing
tab no_antenal_visits // 2,095 don't know; no visit 10,053
keep if no_antenal_visits != .

*ANC in first trimester*
count if time_first_anc == . //
tab time_first_anc if no_antenal_visits == . //0 observation so GOOD

gen ANC_first_trimester = 1 if time_first_anc <= 3
replace ANC_first_trimester = 0 if time_first_anc >= 4

* atleast 4 ANC*
gen ANC_atleast_four2 = 1 if no_antenal_visits >= 4
replace ANC_atleast_four2 = 0 if no_antenal_visits < 4


* tetanus injection *
count if tetanus_number == . //0 missing
gen atleast_one_tet = 1 if tetanus_number >=1
replace atleast_one_tet = 0 if atleast_one_tet != 1  

gen atleast_two_tet = 1 if tetanus_number >=2
replace atleast_two_tet = 0 if atleast_two_tet != 1  

gen atleast_five_tet = 1 if tetanus_number >=5
replace atleast_five_tet = 0 if atleast_five_tet != 1  

* folic acid *
count if preg_iron_tab == . // 0 missing
count if preg_iron_days == . // 0 missing

gen iron_100 = 1 if preg_iron_days >= 100
replace iron_100 = 0 if iron_100 != 1

gen iron_180 = 1 if preg_iron_days >= 180
replace iron_180 = 0 if iron_180 != 1

** indicator 2: INSTITUTIONAL BIRTHS **
* place of delivery *

count if place_of_delivery == .
gen institutional_birth = 0 if place_of_delivery == 10|place_of_delivery == 11|/*
*/							   place_of_delivery == 12|place_of_delivery == 13|/*
*/                             place_of_delivery == 96

replace institutional_birth = 1 if institutional_birth != 0

* place of delivery PUBLIC *

count if institutional_birth == .
gen institutional_birth_public = 1 if place_of_delivery == 20|place_of_delivery == 21|/*
*/							   place_of_delivery == 22|place_of_delivery == 23|/*
*/                             place_of_delivery == 24|place_of_delivery == 25|/*
*/                             place_of_delivery == 26|place_of_delivery == 27|/*
*/                             place_of_delivery == 33

replace institutional_birth_public = 0 if institutional_birth_public != 1

* skilled birth attendant *
count if assist_doc ==.
gen skilled_birth_attend = 1 if assist_doc ==1|assist_midw == 1|/*
*/								assist_other_hp == 1
replace skilled_birth_attend = 0 if skilled_birth_attend != 1

* caesarean *
count if  caesarean ==.

** indicator 3 : POST NATAL CARE **

* first two days pnc *
count if  pnc_time_after == . //83,897 missing
gen pnc_first_twodays = 1 if  pnc_time_after <= 202 //doubtful here 
replace pnc_first_twodays = 0 if pnc_first_twodays !=1

* health_check_postdel*
count if health_check_postdel ==. // 0 missing

* baby_pnc *
count if baby_pnc ==. // 0 missing

*****************************************************************
*expenditure total variable*

count if total_cost_oop ==. // 103989 missing 
fillmissing total_cost_oop, with(mean) // impute missing values w mean 


** check health insurance var***
count if health_insur ==. // 0 missing 

** religion *
gen hindu = 1 if religion == 1
replace hindu = 0 if hindu != 1

gen muslim = 1 if religion == 2
replace muslim = 0 if muslim != 1

gen christian = 1 if religion == 3
replace christian = 0 if christian != 1

gen other_rel2 = 1 if religion != 1 & religion !=2& religion != 3
replace other_rel2 = 0 if other_rel2 != 1

*freq-radio*
 gen freq_radio2 = 1 if freq_radio != 0
replace freq_radio2 = 0 if freq_radio2 != 1

** education **
 gen no_educ = 1 if ind_high_education == 0
 replace no_educ = 0 if no_educ != 1
 
 gen primary_educ = 1 if ind_high_education == 1
 replace primary_educ = 0 if primary_educ != 1

 gen sec_educ = 1 if ind_high_education == 2 
 replace sec_educ = 0 if sec_educ != 1
 
 gen high_educ3 = 1 if ind_high_education == 3
 replace high_educ3 = 0 if high_educ3 != 1
 
 ** type of residence **
  gen urban = 1 if type_of_residence == 1
  replace urban = 0 if type_of_residence == 2
 
  gen rural = 1 if type_of_residence == 2
  replace rural = 0 if type_of_residence == 1
  
  ** hh head sex ** 
  gen male_head = 1 if hh_head_sex ==1
  replace male_head =0 if male_head !=1
  
  gen female_head = 1 if hh_head_sex ==2
  replace female_head = 0 if female_head !=1
  
  gen trans_head = 1 if hh_head_sex == 3
  replace trans_head = 0 if trans_head != 1
 
 //////////////////////////////////////////////////////////////
 ** Create region variable
 *north--2,3,5,9,6, 
 *south-- 28,36,29,32,33
 *east-- 10,20,19,21
 *west-- 8,30,24,27
 *north-east -- 18,11, 13,17,14,15,16,12 
 *central-- 22,23
 *UTs-- rest
 
 gen region = 1 if state == 2|state == 3|state == 5|state == 9|state == 6 //north
 replace region = 2 if state == 28|state == 36|state == 29|state == 32|state == 33 //south
 replace region = 3 if state == 10|state == 20|state == 19|state == 21 // east
 replace region = 4 if state == 8|state == 30|state == 24|state == 27 //west
 replace region = 5 if state == 18|state == 11|state == 13|state == 17|state == 14|state == 15|state == 16|state == 12 //northeast
 replace region = 6 if state == 22| state == 23 //central
 replace region = 0 if region ==. //UTS
 
gen type_of_insurance = 1 if health_insur_cghs == 1
replace type_of_insurance = 2 if health_insur_chip == 1 & type_of_insurance != 1
 replace type_of_insurance = 3 if health_insur_empl1 == 1 & type_of_insurance != 1
 replace type_of_insurance = 4 if health_insur_empl2 == 1 & type_of_insurance != 1
 replace type_of_insurance = 5 if health_insur_esis == 1 & type_of_insurance != 1
 replace type_of_insurance = 7 if health_insur_pvt == 1 & type_of_insurance != 1
 replace type_of_insurance = 7 if health_insur_rsby == 1 & type_of_insurance != 1
 replace type_of_insurance = 8 if health_insur_state == 1 & type_of_insurance != 1
 replace type_of_insurance = 9 if type_of_insurance ==. & health_insur == 1
  replace type_of_insurance = 0 if type_of_insurance ==. 

 gen overall_ANC = 1 if ANC_atleast_four2 == 1 & atleast_two_tet ==1 & iron_100 ==1
replace overall_ANC = 0 if overall_ANC != 1

** gen total maternal health care (all three) **
gen total_maternal_healthcare1 =  1 if overall_ANC == 1 & institutional_birth ==1 & health_check_postdel ==1
replace total_maternal_healthcare1 = 0 if total_maternal_healthcare1 !=1

** gen total maternal health care (two on three) **
gen total_maternal_healthcare2 =  1 if overall_ANC == 1 & institutional_birth ==1 | overall_ANC == 1& health_check_postdel ==1| institutional_birth ==1 & health_check_postdel ==1
replace total_maternal_healthcare2 = 0 if total_maternal_healthcare2 !=1

 
 
 save "C:\Users\Rajsi Sah\Desktop\NFHS-5\Data Cleaning\EHE NFHS-5 Data\cleaned_data_stata_vf.dta", replace
 

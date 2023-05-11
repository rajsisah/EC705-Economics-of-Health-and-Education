///////////////////////////////////////////////////////////////
******PROPENSITY SCORE MATCHING*********
**on antenatal care**
// no_antenal_visits, atleast_one_tet, atleast_two_tet, 
// atleast_five_tet, iron_100


**first scheck balancing**
// ideal covariates: 

** this is for no antenatal visits **

psmatch2 health_insur i.type_of_residence i.ind_high_education i.religion i.freq_newspaper i.freq_radio2 i.freq_tv i.wealth_index_tot/*
 */                        i.hh_head_sex i.bank_account , out( no_antenal_visits ) common logit cal(0.02)
// result is increase by wpprox 1 day

psgraph

pstest type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion, label graph

** this is for ANC_first_trimester **

psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out( ANC_first_trimester )  common logit cal(0.02) 
// result is increase by 0.04 in a scale of 0 to 1 (interpret as percent likely)

psgraph

** this is for atleast 4 visits ANC_atleast_four2 ** 
psmatch2 health_insur i.type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out( ANC_atleast_four2 ) common logit cal(0.02)
// result is increase by 0.107 in a scale of 0 to 1

** this is for atleast_two_tet ** 
psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out( atleast_two_tet) common logit cal(0.02)
// result is increase by 0.012 in a scale of 0 to 1---- insignificant (1.34)

* atleast  doses**

** this is for atleast_one_tet ** 
psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out( atleast_one_tet) common logit cal(0.02)
// result is increase by 0.0107 in a scale of 0 to 1----- insignificant (1.24)

** this is for atleast_two_tet ** 
psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out( atleast_two_tet) common logit cal(0.02)

** this is for iron_100 ** 
psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(iron_100) common logit cal(0.02)
// result is increase by 0.0719 in a scale of 0 to 1

** this is for iron_180 ** 
psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(iron_180) common logit cal(0.02)
// result is increase by 0.0023 in a scale of 0 to 1----- insignificant (0.13)

psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(overall_ANC) common logit cal(0.02)
// result is increase by 0.087 in a scale of 0 to 1

**on institutional delivery**
// institutional_birth, institutional_birth_public, skilled_birth_attend, caesarean

	psmatch2 health_insur type_of_residence /*
	*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
	*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
	*/ bank_account i.religion , out(institutional_birth) common logit cal(0.02)
// result is increase by 0.003 in a scale of 0 to 1----- insignificant (0.30)

psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(institutional_birth_public) common logit cal(0.02)
// result is increase by -0.009 ----- insignificant (-0.53)

psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(skilled_birth_attend) common logit cal(0.02)
// result is increase by 0.009 ----- insignificant (0.74)

psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(caesarean) common logit cal(0.02)
// result is increase by -0.007 ----- insignificant (-0.49)

** on post-natal care **

psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(pnc_first_twodays) common logit cal(0.02)
/// not report this ////


psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(health_check_postdel) common logit cal(0.02)


psmatch2 health_insur type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(baby_pnc) common logit cal(0.02)


psmatch2 health_insur i.type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(birth_ht_cm) common logit cal(0.02)


psmatch2 health_insur i.type_of_residence /*
*/ i.hh_head_sex i.ind_high_education nn_hh_children  /*
*/ i.wealth_index_tot birth_order i.freq_newspaper i.freq_radio i.freq_tv  /*
*/ bank_account i.religion , out(total_maternal_healthcare2) common logit cal(0.02)

///////////////////////////////////////////////////////////////

args = commandArgs(trailingOnly=TRUE)
library(stringr)
library(readr)
library(dplyr)


new_bd = read_tsv(args[1],col_types=cols_only(
EID = col_character(),
Sex.0.0 = col_character(),
`Month of birth.0.0` = col_character(),
`Year of birth.0.0` = col_double(),
`Place of birth in UK - north co-ordinate.0.0` = col_double(),
`Place of birth in UK - north co-ordinate.1.0` = col_double(),
`Place of birth in UK - north co-ordinate.2.0` = col_double(),
`Townsend deprivation index at recruitment.0.0` = col_double(),
`Treatment/medication code.0.0` = col_character(),
`Myopia diagnosis.0.0`=col_character(),
`Age completed full time education.0.0` = col_double(),
`Age at recruitment.0.0` = col_double(),
`Age completed full time education.1.0` = col_double(),
`Age completed full time education.2.0` = col_double(),
`Sleep duration.0.0` = col_double(),
`Sleep duration.1.0` = col_double(),
`Sleep duration.2.0` = col_double(),
`Sleep duration.3.0` = col_double(),
`Getting up in morning.0.0` = col_character(),
`Getting up in morning.1.0` = col_character(),
`Getting up in morning.2.0` = col_character(),
`Getting up in morning.3.0` = col_character(),
`Morning/evening person (chronotype).0.0` = col_character(),
`Morning/evening person (chronotype).1.0` = col_character(),
`Morning/evening person (chronotype).2.0` = col_character(),
`Morning/evening person (chronotype).3.0` = col_character(),
`Nap during day.0.0` = col_character(),
`Nap during day.1.0` = col_character(),
`Nap during day.2.0` = col_character(),
`Nap during day.3.0` = col_character(),
`Sleeplessness / insomnia.0.0` = col_character(),
`Sleeplessness / insomnia.1.0` = col_character(),
`Sleeplessness / insomnia.2.0` = col_character(),
`Sleeplessness / insomnia.3.0` = col_character(),
Snoring.0.0 = col_character(),
Snoring.1.0 = col_character(),
Snoring.2.0 = col_character(),
Snoring.3.0 = col_character(),
`Daytime dozing / sleeping (narcolepsy).0.0` = col_character(),
`Daytime dozing / sleeping (narcolepsy).1.0` = col_character(),
`Daytime dozing / sleeping (narcolepsy).2.0` = col_character(),
`Daytime dozing / sleeping (narcolepsy).3.0` = col_character(),
`Current tobacco smoking.0.0` = col_character(),
`Current tobacco smoking.1.0` = col_character(),
`Current tobacco smoking.2.0` = col_character(),
`Current tobacco smoking.3.0` = col_character(),
`Past tobacco smoking.0.0` = col_character(),
`Past tobacco smoking.1.0` = col_character(),
`Past tobacco smoking.2.0` = col_character(),
`Past tobacco smoking.3.0` = col_character(),
`Smoking/smokers in household.0.0` = col_character(),
`Smoking/smokers in household.1.0` = col_character(),
`Smoking/smokers in household.2.0` = col_character(),
`Smoking/smokers in household.3.0` = col_character(),
`Exposure to tobacco smoke at home.0.0` = col_double(),
`Exposure to tobacco smoke at home.1.0` = col_double(),
`Exposure to tobacco smoke at home.2.0` = col_double(),
`Exposure to tobacco smoke at home.3.0` = col_double(),
`Exposure to tobacco smoke outside home.0.0` = col_double(),
`Exposure to tobacco smoke outside home.1.0` = col_double(),
`Exposure to tobacco smoke outside home.2.0` = col_double(),
`Exposure to tobacco smoke outside home.3.0` = col_double(),
`Oily fish intake.0.0` = col_character(),
`Oily fish intake.1.0` = col_character(),
`Oily fish intake.2.0` = col_character(),
`Oily fish intake.3.0` = col_character(),
`Non-oily fish intake.0.0` = col_character(),
`Non-oily fish intake.1.0` = col_character(),
`Non-oily fish intake.2.0` = col_character(),
`Non-oily fish intake.3.0` = col_character(),
`Alcohol intake frequency..0.0` = col_character(),
`Alcohol intake frequency..1.0` = col_character(),
`Alcohol intake frequency..2.0` = col_character(),
`Alcohol intake frequency..3.0` = col_character(),
`Country of birth (UK/elsewhere).0.0` = col_character(),
`Country of birth (UK/elsewhere).1.0` = col_character(),
`Country of birth (UK/elsewhere).2.0` = col_character(),
`Breastfed as a baby.0.0` = col_character(),
`Breastfed as a baby.1.0` = col_character(),
`Breastfed as a baby.2.0` = col_character(),
`Comparative body size at age 10.0.0` = col_character(),
`Comparative body size at age 10.1.0` = col_character(),
`Comparative body size at age 10.2.0` = col_character(),
`Comparative height size at age 10.0.0` = col_character(),
`Comparative height size at age 10.1.0` = col_character(),
`Comparative height size at age 10.2.0` = col_character(),
`Handedness (chirality/laterality).0.0` = col_character(),
`Handedness (chirality/laterality).1.0` = col_character(),
`Handedness (chirality/laterality).2.0` = col_character(),
`Childhood sunburn occasions.0.0` = col_double(),
`Childhood sunburn occasions.1.0` = col_double(),
`Childhood sunburn occasions.2.0` = col_double(),
`Part of a multiple birth.0.0` = col_character(),
`Part of a multiple birth.1.0` = col_character(),
`Part of a multiple birth.2.0` = col_character(),
`Maternal smoking around birth.0.0` = col_character(),
`Maternal smoking around birth.1.0` = col_character(),
`Maternal smoking around birth.2.0` = col_character(),
`Age first had sexual intercourse.0.0` = col_double(),
`Age first had sexual intercourse.1.0` = col_double(),
`Age first had sexual intercourse.2.0` = col_double(),
`Age first had sexual intercourse.3.0` = col_double(),
`Relative age of first facial hair.0.0` = col_character(),
`Relative age of first facial hair.1.0` = col_character(),
`Relative age of first facial hair.2.0` = col_character(),
`Relative age voice broke.0.0` = col_character(),
`Relative age voice broke.1.0` = col_character(),
`Relative age voice broke.2.0` = col_character(),
`Age when periods started (menarche).0.0` = col_double(),
`Age when periods started (menarche).1.0` = col_double(),
`Age when periods started (menarche).2.0` = col_double(),
`Age when periods started (menarche).3.0` = col_double(),
`Age started smoking in former smokers.0.0` = col_double(),
`Age started smoking in former smokers.1.0` = col_double(),
`Age started smoking in former smokers.2.0` = col_double(),
`Age started smoking in former smokers.3.0` = col_double(),
`Age stopped smoking.0.0` = col_double(),
`Age stopped smoking.1.0` = col_double(),
`Age stopped smoking.2.0` = col_double(),
`Age stopped smoking.3.0` = col_double(),
`Age started smoking in current smokers.0.0` = col_double(),
`Age started smoking in current smokers.1.0` = col_double(),
`Age started smoking in current smokers.2.0` = col_double(),
`Age started smoking in current smokers.3.0` = col_double(),
`Attendance/disability/mobility allowance.0.0` = col_character(),
`Attendance/disability/mobility allowance.0.1` = col_character(),
`Attendance/disability/mobility allowance.0.2` = col_character(),
`Vitamin and mineral supplements.0.0` = col_character(),
`Vitamin and mineral supplements.0.1` = col_character(),
`Vitamin and mineral supplements.0.2` = col_character(),
`Vitamin and mineral supplements.0.3` = col_character(),
`Vitamin and mineral supplements.0.4` = col_character(),
`Vitamin and mineral supplements.0.5` = col_character(),
`Vitamin and mineral supplements.0.6` = col_character(),
`Vitamin and mineral supplements.1.0` = col_character(),
`Vitamin and mineral supplements.1.1` = col_character(),
`Vitamin and mineral supplements.1.2` = col_character(),
`Vitamin and mineral supplements.1.3` = col_character(),
`Vitamin and mineral supplements.1.4` = col_character(),
`Vitamin and mineral supplements.1.5` = col_character(),
`Vitamin and mineral supplements.1.6` = col_character(),
`Vitamin and mineral supplements.2.0` = col_character(),
`Vitamin and mineral supplements.2.1` = col_character(),
`Vitamin and mineral supplements.2.2` = col_character(),
`Vitamin and mineral supplements.2.3` = col_character(),
`Vitamin and mineral supplements.2.4` = col_character(),
`Vitamin and mineral supplements.2.5` = col_character(),
`Vitamin and mineral supplements.2.6` = col_character(),
`Vitamin and mineral supplements.3.0` = col_character(),
`Vitamin and mineral supplements.3.1` = col_character(),
`Vitamin and mineral supplements.3.2` = col_character(),
`Vitamin and mineral supplements.3.3` = col_character(),
`Vitamin and mineral supplements.3.4` = col_character(),
`Vitamin and mineral supplements.3.5` = col_character(),
`Vitamin and mineral supplements.3.6` = col_logical(),
`Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker).0.0` = col_double(),
`Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker).1.0` = col_double(),
`Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker).2.0` = col_double(),
`Non-cancer illness code, self-reported.0.0` = col_double(),
`Non-cancer illness code, self-reported.0.1` = col_double(),
`Non-cancer illness code, self-reported.0.2` = col_double(),
`Non-cancer illness code, self-reported.0.3` = col_double(),
`Non-cancer illness code, self-reported.0.4` = col_double(),
`Non-cancer illness code, self-reported.0.5` = col_double(),
`Non-cancer illness code, self-reported.0.6` = col_double(),
`Non-cancer illness code, self-reported.0.7` = col_double(),
`Non-cancer illness code, self-reported.0.8` = col_double(),
`Non-cancer illness code, self-reported.0.9` = col_double(),
`Non-cancer illness code, self-reported.0.10` = col_double(),
`Non-cancer illness code, self-reported.0.11` = col_double(),
`Non-cancer illness code, self-reported.0.12` = col_double(),
`Non-cancer illness code, self-reported.0.13` = col_double(),
`Non-cancer illness code, self-reported.0.14` = col_double(),
`Non-cancer illness code, self-reported.0.15` = col_double(),
`Non-cancer illness code, self-reported.0.16` = col_logical(),
`Non-cancer illness code, self-reported.0.17` = col_logical(),
`Non-cancer illness code, self-reported.0.18` = col_logical(),
`Non-cancer illness code, self-reported.0.19` = col_logical(),
`Non-cancer illness code, self-reported.0.20` = col_logical(),
`Non-cancer illness code, self-reported.0.21` = col_logical(),
`Non-cancer illness code, self-reported.0.22` = col_logical(),
`Non-cancer illness code, self-reported.0.23` = col_logical(),
`Non-cancer illness code, self-reported.0.24` = col_logical(),
`Non-cancer illness code, self-reported.0.25` = col_logical(),
`Non-cancer illness code, self-reported.0.26` = col_logical(),
`Non-cancer illness code, self-reported.0.27` = col_logical(),
`Non-cancer illness code, self-reported.0.28` = col_logical(),
`Non-cancer illness code, self-reported.0.29` = col_logical(),
`Non-cancer illness code, self-reported.0.30` = col_logical(),
`Non-cancer illness code, self-reported.0.31` = col_logical(),
`Non-cancer illness code, self-reported.0.32` = col_logical(),
`Non-cancer illness code, self-reported.0.33` = col_logical(),
`Non-cancer illness code, self-reported.1.0` = col_double(),
`Non-cancer illness code, self-reported.1.1` = col_double(),
`Non-cancer illness code, self-reported.1.2` = col_double(),
`Non-cancer illness code, self-reported.1.3` = col_double(),
`Non-cancer illness code, self-reported.1.4` = col_double(),
`Non-cancer illness code, self-reported.1.5` = col_double(),
`Non-cancer illness code, self-reported.1.6` = col_double(),
`Non-cancer illness code, self-reported.1.7` = col_double(),
`Non-cancer illness code, self-reported.1.8` = col_double(),
`Non-cancer illness code, self-reported.1.9` = col_double(),
`Non-cancer illness code, self-reported.1.10` = col_double(),
`Non-cancer illness code, self-reported.1.11` = col_double(),
`Non-cancer illness code, self-reported.1.12` = col_double(),
`Non-cancer illness code, self-reported.1.13` = col_double(),
`Non-cancer illness code, self-reported.1.14` = col_logical(),
`Non-cancer illness code, self-reported.1.15` = col_logical(),
`Non-cancer illness code, self-reported.1.16` = col_logical(),
`Non-cancer illness code, self-reported.1.17` = col_logical(),
`Non-cancer illness code, self-reported.1.18` = col_logical(),
`Non-cancer illness code, self-reported.1.19` = col_logical(),
`Non-cancer illness code, self-reported.1.20` = col_logical(),
`Non-cancer illness code, self-reported.1.21` = col_logical(),
`Non-cancer illness code, self-reported.1.22` = col_logical(),
`Non-cancer illness code, self-reported.1.23` = col_logical(),
`Non-cancer illness code, self-reported.1.24` = col_logical(),
`Non-cancer illness code, self-reported.1.25` = col_logical(),
`Non-cancer illness code, self-reported.1.26` = col_logical(),
`Non-cancer illness code, self-reported.1.27` = col_logical(),
`Non-cancer illness code, self-reported.1.28` = col_logical(),
`Non-cancer illness code, self-reported.1.29` = col_logical(),
`Non-cancer illness code, self-reported.1.30` = col_logical(),
`Non-cancer illness code, self-reported.1.31` = col_logical(),
`Non-cancer illness code, self-reported.1.32` = col_logical(),
`Non-cancer illness code, self-reported.1.33` = col_logical(),
`Non-cancer illness code, self-reported.2.0` = col_double(),
`Non-cancer illness code, self-reported.2.1` = col_double(),
`Non-cancer illness code, self-reported.2.2` = col_double(),
`Non-cancer illness code, self-reported.2.3` = col_double(),
`Non-cancer illness code, self-reported.2.4` = col_double(),
`Non-cancer illness code, self-reported.2.5` = col_double(),
`Non-cancer illness code, self-reported.2.6` = col_double(),
`Non-cancer illness code, self-reported.2.7` = col_double(),
`Non-cancer illness code, self-reported.2.8` = col_double(),
`Non-cancer illness code, self-reported.2.9` = col_double(),
`Non-cancer illness code, self-reported.2.10` = col_double(),
`Non-cancer illness code, self-reported.2.11` = col_double(),
`Non-cancer illness code, self-reported.2.12` = col_double(),
`Non-cancer illness code, self-reported.2.13` = col_double(),
`Non-cancer illness code, self-reported.2.14` = col_double(),
`Non-cancer illness code, self-reported.2.15` = col_double(),
`Non-cancer illness code, self-reported.2.16` = col_double(),
`Non-cancer illness code, self-reported.2.17` = col_double(),
`Non-cancer illness code, self-reported.2.18` = col_double(),
`Non-cancer illness code, self-reported.2.19` = col_double(),
`Non-cancer illness code, self-reported.2.20` = col_logical(),
`Non-cancer illness code, self-reported.2.21` = col_logical(),
`Non-cancer illness code, self-reported.2.22` = col_logical(),
`Non-cancer illness code, self-reported.2.23` = col_logical(),
`Non-cancer illness code, self-reported.2.24` = col_logical(),
`Non-cancer illness code, self-reported.2.25` = col_logical(),
`Non-cancer illness code, self-reported.2.26` = col_logical(),
`Non-cancer illness code, self-reported.2.27` = col_logical(),
`Non-cancer illness code, self-reported.2.28` = col_logical(),
`Non-cancer illness code, self-reported.2.29` = col_logical(),
`Non-cancer illness code, self-reported.2.30` = col_logical(),
`Non-cancer illness code, self-reported.2.31` = col_logical(),
`Non-cancer illness code, self-reported.2.32` = col_logical(),
`Non-cancer illness code, self-reported.2.33` = col_logical(),
`Non-cancer illness code, self-reported.3.0` = col_double(),
`Non-cancer illness code, self-reported.3.1` = col_double(),
`Non-cancer illness code, self-reported.3.2` = col_double(),
`Non-cancer illness code, self-reported.3.3` = col_double(),
`Non-cancer illness code, self-reported.3.4` = col_double(),
`Non-cancer illness code, self-reported.3.5` = col_double(),
`Non-cancer illness code, self-reported.3.6` = col_double(),
`Non-cancer illness code, self-reported.3.7` = col_double(),
`Non-cancer illness code, self-reported.3.8` = col_double(),
`Non-cancer illness code, self-reported.3.9` = col_logical(),
`Non-cancer illness code, self-reported.3.10` = col_logical(),
`Non-cancer illness code, self-reported.3.11` = col_logical(),
`Non-cancer illness code, self-reported.3.12` = col_logical(),
`Non-cancer illness code, self-reported.3.13` = col_logical(),
`Non-cancer illness code, self-reported.3.14` = col_logical(),
`Non-cancer illness code, self-reported.3.15` = col_logical(),
`Non-cancer illness code, self-reported.3.16` = col_logical(),
`Non-cancer illness code, self-reported.3.17` = col_logical(),
`Non-cancer illness code, self-reported.3.18` = col_logical(),
`Non-cancer illness code, self-reported.3.19` = col_logical(),
`Non-cancer illness code, self-reported.3.20` = col_logical(),
`Non-cancer illness code, self-reported.3.21` = col_logical(),
`Non-cancer illness code, self-reported.3.22` = col_logical(),
`Non-cancer illness code, self-reported.3.23` = col_logical(),
`Non-cancer illness code, self-reported.3.24` = col_logical(),
`Non-cancer illness code, self-reported.3.25` = col_logical(),
`Non-cancer illness code, self-reported.3.26` = col_logical(),
`Non-cancer illness code, self-reported.3.27` = col_logical(),
`Non-cancer illness code, self-reported.3.28` = col_logical(),
`Non-cancer illness code, self-reported.3.29` = col_logical(),
`Non-cancer illness code, self-reported.3.30` = col_logical(),
`Non-cancer illness code, self-reported.3.31` = col_logical(),
`Non-cancer illness code, self-reported.3.32` = col_logical(),
`Non-cancer illness code, self-reported.3.33` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.0` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.1` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.2` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.3` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.4` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.5` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.6` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.7` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.8` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.9` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.10` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.11` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.12` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.13` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.14` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.15` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.16` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.17` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.18` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.19` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.20` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.21` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.22` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.23` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.24` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.25` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.26` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.27` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.28` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.29` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.30` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.31` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.32` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.0.33` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.0` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.1` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.2` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.3` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.4` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.5` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.6` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.7` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.8` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.9` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.10` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.11` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.12` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.13` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.14` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.15` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.16` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.17` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.18` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.19` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.20` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.21` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.22` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.23` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.24` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.25` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.26` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.27` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.28` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.29` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.30` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.31` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.32` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.1.33` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.0` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.1` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.2` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.3` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.4` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.5` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.6` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.7` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.8` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.9` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.10` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.11` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.12` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.13` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.14` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.15` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.16` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.17` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.18` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.19` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.20` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.21` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.22` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.23` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.24` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.25` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.26` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.27` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.28` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.29` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.30` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.31` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.32` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.2.33` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.0` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.1` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.2` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.3` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.4` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.5` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.6` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.7` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.8` = col_double(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.9` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.10` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.11` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.12` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.13` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.14` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.15` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.16` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.17` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.18` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.19` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.20` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.21` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.22` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.23` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.24` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.25` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.26` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.27` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.28` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.29` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.30` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.31` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.32` = col_logical(),
`Interpolated Age of participant when non-cancer illness first diagnosed.3.33` = col_logical(),
`Birth weight.0.0` = col_double(),
`Birth weight.1.0` = col_double(),
`Birth weight.2.0` = col_double(),
`Illnesses of father.0.0` = col_character(),
`Illnesses of father.0.1` = col_character(),
`Illnesses of father.0.2` = col_character(),
`Illnesses of father.0.3` = col_character(),
`Illnesses of father.0.4` = col_character(),
`Illnesses of father.0.5` = col_character(),
`Illnesses of father.0.6` = col_character(),
`Illnesses of father.0.7` = col_logical(),
`Illnesses of father.0.8` = col_logical(),
`Illnesses of father.0.9` = col_logical(),
`Illnesses of father.1.0` = col_character(),
`Illnesses of father.1.1` = col_character(),
`Illnesses of father.1.2` = col_character(),
`Illnesses of father.1.3` = col_character(),
`Illnesses of father.1.4` = col_character(),
`Illnesses of father.1.5` = col_logical(),
`Illnesses of father.1.6` = col_logical(),
`Illnesses of father.1.7` = col_logical(),
`Illnesses of father.1.8` = col_logical(),
`Illnesses of father.1.9` = col_logical(),
`Illnesses of father.2.0` = col_character(),
`Illnesses of father.2.1` = col_character(),
`Illnesses of father.2.2` = col_character(),
`Illnesses of father.2.3` = col_character(),
`Illnesses of father.2.4` = col_character(),
`Illnesses of father.2.5` = col_logical(),
`Illnesses of father.2.6` = col_logical(),
`Illnesses of father.2.7` = col_logical(),
`Illnesses of father.2.8` = col_logical(),
`Illnesses of father.2.9` = col_logical(),
`Illnesses of father.3.0` = col_character(),
`Illnesses of father.3.1` = col_character(),
`Illnesses of father.3.2` = col_character(),
`Illnesses of father.3.3` = col_character(),
`Illnesses of father.3.4` = col_logical(),
`Illnesses of father.3.5` = col_logical(),
`Illnesses of father.3.6` = col_logical(),
`Illnesses of father.3.7` = col_logical(),
`Illnesses of father.3.8` = col_logical(),
`Illnesses of father.3.9` = col_logical(),
`Illnesses of mother.0.0` = col_character(),
`Illnesses of mother.0.1` = col_character(),
`Illnesses of mother.0.2` = col_character(),
`Illnesses of mother.0.3` = col_character(),
`Illnesses of mother.0.4` = col_character(),
`Illnesses of mother.0.5` = col_character(),
`Illnesses of mother.0.6` = col_logical(),
`Illnesses of mother.0.7` = col_logical(),
`Illnesses of mother.0.8` = col_logical(),
`Illnesses of mother.0.9` = col_logical(),
`Illnesses of mother.0.10` = col_logical(),
`Illnesses of mother.1.0` = col_character(),
`Illnesses of mother.1.1` = col_character(),
`Illnesses of mother.1.2` = col_character(),
`Illnesses of mother.1.3` = col_character(),
`Illnesses of mother.1.4` = col_character(),
`Illnesses of mother.1.5` = col_character(),
`Illnesses of mother.1.6` = col_logical(),
`Illnesses of mother.1.7` = col_logical(),
`Illnesses of mother.1.8` = col_logical(),
`Illnesses of mother.1.9` = col_logical(),
`Illnesses of mother.1.10` = col_logical(),
`Illnesses of mother.2.0` = col_character(),
`Illnesses of mother.2.1` = col_character(),
`Illnesses of mother.2.2` = col_character(),
`Illnesses of mother.2.3` = col_character(),
`Illnesses of mother.2.4` = col_character(),
`Illnesses of mother.2.5` = col_character(),
`Illnesses of mother.2.6` = col_logical(),
`Illnesses of mother.2.7` = col_logical(),
`Illnesses of mother.2.8` = col_logical(),
`Illnesses of mother.2.9` = col_logical(),
`Illnesses of mother.2.10` = col_logical(),
`Illnesses of mother.3.0` = col_character(),
`Illnesses of mother.3.1` = col_character(),
`Illnesses of mother.3.2` = col_character(),
`Illnesses of mother.3.3` = col_logical(),
`Illnesses of mother.3.4` = col_logical(),
`Illnesses of mother.3.5` = col_logical(),
`Illnesses of mother.3.6` = col_logical(),
`Illnesses of mother.3.7` = col_logical(),
`Illnesses of mother.3.8` = col_logical(),
`Illnesses of mother.3.9` = col_logical(),
`Illnesses of mother.3.10` = col_logical(),
`Illnesses of siblings.0.0` = col_character(),
`Illnesses of siblings.0.1` = col_character(),
`Illnesses of siblings.0.2` = col_character(),
`Illnesses of siblings.0.3` = col_character(),
`Illnesses of siblings.0.4` = col_character(),
`Illnesses of siblings.0.5` = col_character(),
`Illnesses of siblings.0.6` = col_character(),
`Illnesses of siblings.0.7` = col_logical(),
`Illnesses of siblings.0.8` = col_logical(),
`Illnesses of siblings.0.9` = col_logical(),
`Illnesses of siblings.0.10` = col_logical(),
`Illnesses of siblings.0.11` = col_logical(),
`Illnesses of siblings.1.0` = col_character(),
`Illnesses of siblings.1.1` = col_character(),
`Illnesses of siblings.1.2` = col_character(),
`Illnesses of siblings.1.3` = col_character(),
`Illnesses of siblings.1.4` = col_character(),
`Illnesses of siblings.1.5` = col_character(),
`Illnesses of siblings.1.6` = col_character(),
`Illnesses of siblings.1.7` = col_character(),
`Illnesses of siblings.1.8` = col_logical(),
`Illnesses of siblings.1.9` = col_logical(),
`Illnesses of siblings.1.10` = col_logical(),
`Illnesses of siblings.1.11` = col_logical(),
`Illnesses of siblings.2.0` = col_character(),
`Illnesses of siblings.2.1` = col_character(),
`Illnesses of siblings.2.2` = col_character(),
`Illnesses of siblings.2.3` = col_character(),
`Illnesses of siblings.2.4` = col_character(),
`Illnesses of siblings.2.5` = col_logical(),
`Illnesses of siblings.2.6` = col_logical(),
`Illnesses of siblings.2.7` = col_logical(),
`Illnesses of siblings.2.8` = col_logical(),
`Illnesses of siblings.2.9` = col_logical(),
`Illnesses of siblings.2.10` = col_logical(),
`Illnesses of siblings.2.11` = col_logical(),
`Illnesses of siblings.3.0` = col_character(),
`Illnesses of siblings.3.1` = col_character(),
`Illnesses of siblings.3.2` = col_character(),
`Illnesses of siblings.3.3` = col_character(),
`Illnesses of siblings.3.4` = col_character(),
`Illnesses of siblings.3.5` = col_logical(),
`Illnesses of siblings.3.6` = col_logical(),
`Illnesses of siblings.3.7` = col_logical(),
`Illnesses of siblings.3.8` = col_logical(),
`Illnesses of siblings.3.9` = col_logical(),
`Illnesses of siblings.3.10` = col_logical(),
`Illnesses of siblings.3.11` = col_logical(),
`Country of Birth (non-UK origin).0.0` = col_double(),
`Smoking status.0.0` = col_character(),
`Smoking status.1.0` = col_character(),
`Smoking status.2.0` = col_character(),
`Smoking status.3.0` = col_character(),
`Alcohol drinker status.0.0` = col_character(),
`Alcohol drinker status.1.0` = col_character(),
`Alcohol drinker status.2.0` = col_character(),
`Alcohol drinker status.3.0` = col_character(),
`Home area population density - urban or rural.0.0` = col_character(),
`Current employment status - corrected.0.0` = col_character(),
`Ever smoked.0.0` = col_character(),
`Ever smoked.1.0` = col_character(),
`Ever smoked.2.0` = col_character(),
`Ever smoked.3.0` = col_character(),
`Pack years of smoking.0.0` = col_double(),
`Pack years of smoking.1.0` = col_double(),
`Pack years of smoking.2.0` = col_double(),
`Pack years of smoking.3.0` = col_double(),
`Ethnic background.0.0` = col_character(),
`Ethnic background.1.0` = col_character(),
`Ethnic background.2.0` = col_character(),
`Body mass index (BMI).0.0` = col_double(),
`Body mass index (BMI).1.0` = col_double(),
`Body mass index (BMI).2.0` = col_double(),
`Body mass index (BMI).3.0` = col_double(),
`Genotype measurement batch.0.0` = col_double(),
`Genetic sex.0.0` = col_character(),
`CEL files.0.0` = col_character(),
Heterozygosity.0.0 = col_double(),
`Heterozygosity, PCA corrected.0.0` = col_double(),
Missingness.0.0 = col_double(),
`Genetic ethnic grouping.0.0` = col_character(),
`Genetic principal components.0.1` = col_double(),
`Genetic principal components.0.2` = col_double(),
`Genetic principal components.0.3` = col_double(),
`Genetic principal components.0.4` = col_double(),
`Genetic principal components.0.5` = col_double(),
`Genetic principal components.0.6` = col_double(),
`Genetic principal components.0.7` = col_double(),
`Genetic principal components.0.8` = col_double(),
`Genetic principal components.0.9` = col_double(),
`Genetic principal components.0.10` = col_double(),
`Genetic principal components.0.11` = col_double(),
`Falls in the last year.0.0`=col_character(),
`Fractured/broken bones in last 5 years.0.0`=col_character(),
`Heel bone mineral density (BMD) T-score, automated.0.0`=col_character(),
`Usual walking pace.0.0`=col_character(),
`Genetic principal components.0.12` = col_double(),
`Genetic principal components.0.13` = col_double(),
`Genetic principal components.0.14` = col_double(),
`Genetic principal components.0.15` = col_double(),
`Genetic principal components.0.16` = col_double(),
`Genetic principal components.0.17` = col_double(),
`Genetic principal components.0.18` = col_double(),
`Genetic principal components.0.19` = col_double(),
`Genetic principal components.0.20` = col_double(),
`Genetic principal components.0.21` = col_double(),
`Genetic principal components.0.22` = col_double(),
`Genetic principal components.0.23` = col_double(),
`Genetic principal components.0.24` = col_double(),
`Genetic principal components.0.25` = col_double(),
`Genetic principal components.0.26` = col_double(),
`Genetic principal components.0.27` = col_double(),
`Genetic principal components.0.28` = col_double(),
`Genetic principal components.0.29` = col_double(),
`Genetic principal components.0.30` = col_double(),
`Genetic principal components.0.31` = col_double(),
`Genetic principal components.0.32` = col_double(),
`Genetic principal components.0.33` = col_double(),
`Genetic principal components.0.34` = col_double(),
`Genetic principal components.0.35` = col_double(),
`Genetic principal components.0.36` = col_double(),
`Genetic principal components.0.37` = col_double(),
`Genetic principal components.0.38` = col_double(),
`Genetic principal components.0.39` = col_double(),
`Genetic principal components.0.40` = col_double(),
`Recommended genomic analysis exclusions.0.0` = col_character(),
`Genetic relatedness pairing.0.0` = col_double(),
`Genetic relatedness pairing.0.1` = col_double(),
`Genetic relatedness pairing.0.2` = col_double(),
`Genetic relatedness pairing.0.3` = col_logical(),
`Genetic relatedness pairing.0.4` = col_logical(),
`Genetic relatedness factor.0.0` = col_double(),
`Genetic relatedness factor.0.1` = col_double(),
`Genetic relatedness factor.0.2` = col_double(),
`Genetic relatedness factor.0.3` = col_logical(),
`Genetic relatedness factor.0.4` = col_logical(),
`Genetic relatedness IBS0.0.0` = col_double(),
`Genetic relatedness IBS0.0.1` = col_double(),
`Genetic relatedness IBS0.0.2` = col_double(),
`Genetic relatedness IBS0.0.3` = col_logical(),
`Genetic relatedness IBS0.0.4` = col_logical(),
`Had menopause.0.0`= col_logical(),
`Genetic relatedness exclusions.0.0` = col_character(),
`Sex chromosome aneuploidy.0.0` = col_character(),
`Used in genetic principal components.0.0` = col_character(),
`Genetic kinship to other participants.0.0` = col_character(),
`Outliers for heterozygosity or missing rate.0.0` = col_character(),
`HLA imputation values.0.0` = col_character(),
`Age of stopping smoking.0.0` = col_double(),
`Workplace full of chemical or other fumes.0.0` = col_character(),
`Workplace full of chemical or other fumes.0.1` = col_character(),
`Workplace full of chemical or other fumes.0.2` = col_character(),
`Workplace full of chemical or other fumes.0.3` = col_character(),
`Workplace full of chemical or other fumes.0.4` = col_character(),
`Workplace full of chemical or other fumes.0.5` = col_character(),
`Workplace full of chemical or other fumes.0.6` = col_character(),
`Workplace full of chemical or other fumes.0.7` = col_character(),
`Workplace full of chemical or other fumes.0.8` = col_character(),
`Workplace full of chemical or other fumes.0.9` = col_character(),
`Workplace full of chemical or other fumes.0.10` = col_character(),
`Workplace full of chemical or other fumes.0.11` = col_character(),
`Workplace full of chemical or other fumes.0.12` = col_character(),
`Workplace full of chemical or other fumes.0.13` = col_character(),
`Workplace full of chemical or other fumes.0.14` = col_character(),
`Workplace full of chemical or other fumes.0.15` = col_character(),
`Workplace full of chemical or other fumes.0.16` = col_character(),
`Workplace full of chemical or other fumes.0.17` = col_character(),
`Workplace full of chemical or other fumes.0.18` = col_logical(),
`Workplace full of chemical or other fumes.0.19` = col_logical(),
`Workplace full of chemical or other fumes.0.20` = col_logical(),
`Workplace full of chemical or other fumes.0.21` = col_logical(),
`Workplace full of chemical or other fumes.0.22` = col_logical(),
`Workplace full of chemical or other fumes.0.23` = col_logical(),
`Workplace full of chemical or other fumes.0.24` = col_logical(),
`Workplace full of chemical or other fumes.0.25` = col_logical(),
`Workplace full of chemical or other fumes.0.26` = col_logical(),
`Workplace full of chemical or other fumes.0.27` = col_logical(),
`Workplace full of chemical or other fumes.0.28` = col_logical(),
`Workplace full of chemical or other fumes.0.29` = col_logical(),
`Workplace full of chemical or other fumes.0.30` = col_logical(),
`Workplace full of chemical or other fumes.0.31` = col_logical(),
`Workplace full of chemical or other fumes.0.32` = col_logical(),
`Workplace full of chemical or other fumes.0.33` = col_logical(),
`Workplace full of chemical or other fumes.0.34` = col_logical(),
`Workplace full of chemical or other fumes.0.35` = col_logical(),
`Workplace full of chemical or other fumes.0.36` = col_logical(),
`Workplace full of chemical or other fumes.0.37` = col_logical(),
`Workplace full of chemical or other fumes.0.38` = col_logical(),
`Workplace full of chemical or other fumes.0.39` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.0` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.1` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.2` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.3` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.4` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.5` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.6` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.7` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.8` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.9` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.10` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.11` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.12` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.13` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.14` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.15` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.16` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.17` = col_character(),
`Workplace had a lot of cigarette smoke from other people smoking.0.18` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.19` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.20` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.21` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.22` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.23` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.24` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.25` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.26` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.27` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.28` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.29` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.30` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.31` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.32` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.33` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.34` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.35` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.36` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.37` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.38` = col_logical(),
`Workplace had a lot of cigarette smoke from other people smoking.0.39` = col_logical(),
`Worked with paints, thinners or glues.0.0` = col_character(),
`Worked with paints, thinners or glues.0.1` = col_character(),
`Worked with paints, thinners or glues.0.2` = col_character(),
`Worked with paints, thinners or glues.0.3` = col_character(),
`Worked with paints, thinners or glues.0.4` = col_character(),
`Worked with paints, thinners or glues.0.5` = col_character(),
`Worked with paints, thinners or glues.0.6` = col_character(),
`Worked with paints, thinners or glues.0.7` = col_character(),
`Worked with paints, thinners or glues.0.8` = col_character(),
`Worked with paints, thinners or glues.0.9` = col_character(),
`Worked with paints, thinners or glues.0.10` = col_character(),
`Worked with paints, thinners or glues.0.11` = col_character(),
`Worked with paints, thinners or glues.0.12` = col_character(),
`Worked with paints, thinners or glues.0.13` = col_character(),
`Worked with paints, thinners or glues.0.14` = col_character(),
`Worked with paints, thinners or glues.0.15` = col_character(),
`Worked with paints, thinners or glues.0.16` = col_character(),
`Worked with paints, thinners or glues.0.17` = col_character(),
`Worked with paints, thinners or glues.0.18` = col_logical(),
`Worked with paints, thinners or glues.0.19` = col_logical(),
`Worked with paints, thinners or glues.0.20` = col_logical(),
`Worked with paints, thinners or glues.0.21` = col_logical(),
`Worked with paints, thinners or glues.0.22` = col_logical(),
`Worked with paints, thinners or glues.0.23` = col_logical(),
`Worked with paints, thinners or glues.0.24` = col_logical(),
`Worked with paints, thinners or glues.0.25` = col_logical(),
`Worked with paints, thinners or glues.0.26` = col_logical(),
`Worked with paints, thinners or glues.0.27` = col_logical(),
`Worked with paints, thinners or glues.0.28` = col_logical(),
`Worked with paints, thinners or glues.0.29` = col_logical(),
`Worked with paints, thinners or glues.0.30` = col_logical(),
`Worked with paints, thinners or glues.0.31` = col_logical(),
`Worked with paints, thinners or glues.0.32` = col_logical(),
`Worked with paints, thinners or glues.0.33` = col_logical(),
`Worked with paints, thinners or glues.0.34` = col_logical(),
`Worked with paints, thinners or glues.0.35` = col_logical(),
`Worked with paints, thinners or glues.0.36` = col_logical(),
`Worked with paints, thinners or glues.0.37` = col_logical(),
`Worked with paints, thinners or glues.0.38` = col_logical(),
`Worked with paints, thinners or glues.0.39` = col_logical(),
`Worked with pesticides.0.0` = col_character(),
`Worked with pesticides.0.1` = col_character(),
`Worked with pesticides.0.2` = col_character(),
`Worked with pesticides.0.3` = col_character(),
`Worked with pesticides.0.4` = col_character(),
`Worked with pesticides.0.5` = col_character(),
`Worked with pesticides.0.6` = col_character(),
`Worked with pesticides.0.7` = col_character(),
`Worked with pesticides.0.8` = col_character(),
`Worked with pesticides.0.9` = col_character(),
`Worked with pesticides.0.10` = col_character(),
`Worked with pesticides.0.11` = col_character(),
`Worked with pesticides.0.12` = col_character(),
`Worked with pesticides.0.13` = col_character(),
`Worked with pesticides.0.14` = col_character(),
`Worked with pesticides.0.15` = col_character(),
`Worked with pesticides.0.16` = col_character(),
`Worked with pesticides.0.17` = col_character(),
`Worked with pesticides.0.18` = col_logical(),
`Worked with pesticides.0.19` = col_logical(),
`Worked with pesticides.0.20` = col_logical(),
`Worked with pesticides.0.21` = col_logical(),
`Worked with pesticides.0.22` = col_logical(),
`Worked with pesticides.0.23` = col_logical(),
`Worked with pesticides.0.24` = col_logical(),
`Worked with pesticides.0.25` = col_logical(),
`Worked with pesticides.0.26` = col_logical(),
`Worked with pesticides.0.27` = col_logical(),
`Worked with pesticides.0.28` = col_logical(),
`Worked with pesticides.0.29` = col_logical(),
`Worked with pesticides.0.30` = col_logical(),
`Worked with pesticides.0.31` = col_logical(),
`Worked with pesticides.0.32` = col_logical(),
`Worked with pesticides.0.33` = col_logical(),
`Worked with pesticides.0.34` = col_logical(),
`Worked with pesticides.0.35` = col_logical(),
`Worked with pesticides.0.36` = col_logical(),
`Worked with pesticides.0.37` = col_logical(),
`Worked with pesticides.0.38` = col_logical(),
`Worked with pesticides.0.39` = col_logical(),
`Workplace had a lot of diesel exhaust.0.0` = col_character(),
`Workplace had a lot of diesel exhaust.0.1` = col_character(),
`Workplace had a lot of diesel exhaust.0.2` = col_character(),
`Workplace had a lot of diesel exhaust.0.3` = col_character(),
`Workplace had a lot of diesel exhaust.0.4` = col_character(),
`Workplace had a lot of diesel exhaust.0.5` = col_character(),
`Workplace had a lot of diesel exhaust.0.6` = col_character(),
`Workplace had a lot of diesel exhaust.0.7` = col_character(),
`Workplace had a lot of diesel exhaust.0.8` = col_character(),
`Workplace had a lot of diesel exhaust.0.9` = col_character(),
`Workplace had a lot of diesel exhaust.0.10` = col_character(),
`Workplace had a lot of diesel exhaust.0.11` = col_character(),
`Workplace had a lot of diesel exhaust.0.12` = col_character(),
`Workplace had a lot of diesel exhaust.0.13` = col_character(),
`Workplace had a lot of diesel exhaust.0.14` = col_character(),
`Workplace had a lot of diesel exhaust.0.15` = col_character(),
`Workplace had a lot of diesel exhaust.0.16` = col_character(),
`Workplace had a lot of diesel exhaust.0.17` = col_character(),
`Workplace had a lot of diesel exhaust.0.18` = col_logical(),
`Workplace had a lot of diesel exhaust.0.19` = col_logical(),
`Workplace had a lot of diesel exhaust.0.20` = col_logical(),
`Workplace had a lot of diesel exhaust.0.21` = col_logical(),
`Workplace had a lot of diesel exhaust.0.22` = col_logical(),
`Workplace had a lot of diesel exhaust.0.23` = col_logical(),
`Workplace had a lot of diesel exhaust.0.24` = col_logical(),
`Workplace had a lot of diesel exhaust.0.25` = col_logical(),
`Workplace had a lot of diesel exhaust.0.26` = col_logical(),
`Workplace had a lot of diesel exhaust.0.27` = col_logical(),
`Workplace had a lot of diesel exhaust.0.28` = col_logical(),
`Workplace had a lot of diesel exhaust.0.29` = col_logical(),
`Workplace had a lot of diesel exhaust.0.30` = col_logical(),
`Workplace had a lot of diesel exhaust.0.31` = col_logical(),
`Workplace had a lot of diesel exhaust.0.32` = col_logical(),
`Workplace had a lot of diesel exhaust.0.33` = col_logical(),
`Workplace had a lot of diesel exhaust.0.34` = col_logical(),
`Workplace had a lot of diesel exhaust.0.35` = col_logical(),
`Workplace had a lot of diesel exhaust.0.36` = col_logical(),
`Workplace had a lot of diesel exhaust.0.37` = col_logical(),
`Workplace had a lot of diesel exhaust.0.38` = col_logical(),
`Workplace had a lot of diesel exhaust.0.39` = col_logical(),
`VCA p18 antigen for Epstein-Barr Virus.0.0` = col_double(),
`VCA p18 antigen for Epstein-Barr Virus.1.0` = col_double(),
`EBNA-1 antigen for Epstein-Barr Virus.0.0` = col_double(),
`EBNA-1 antigen for Epstein-Barr Virus.1.0` = col_double(),
`ZEBRA antigen for Epstein-Barr Virus.0.0` = col_double(),
`ZEBRA antigen for Epstein-Barr Virus.1.0` = col_double(),
`EA-D antigen for Epstein-Barr Virus.0.0` = col_double(),
`EA-D antigen for Epstein-Barr Virus.1.0` = col_double(),
`Date of attending assessment centre.0.0` = col_date(format = ""),
`SHBG assay date.0.0` = col_date(format = ""),
`SHBG assay date.1.0` = col_date(format = ""),
`SHBG aliquot.0.0` = col_character(),
`SHBG aliquot.1.0` = col_character(),
`SHBG correction level.0.0` = col_character(),
`SHBG correction level.1.0` = col_character(),
`SHBG correction reason.0.0` = col_character(),
`SHBG correction reason.1.0` = col_character(),
`SHBG missing reason.0.0` = col_character(),
`SHBG missing reason.1.0` = col_character(),
`SHBG reportability.0.0` = col_character(),
`SHBG reportability.1.0` = col_character(),
`Testosterone assay date.0.0` = col_date(format = ""),
`Testosterone assay date.1.0` = col_date(format = ""),
`Testosterone aliquot.0.0` = col_character(),
`Testosterone aliquot.1.0` = col_character(),
`Testosterone correction level.0.0` = col_character(),
`Testosterone correction level.1.0` = col_character(),
`Testosterone correction reason.0.0` = col_character(),
`Testosterone correction reason.1.0` = col_character(),
`Testosterone missing reason.0.0` = col_character(),
`Testosterone missing reason.1.0` = col_character(),
`Testosterone reportability.0.0` = col_character(),
`Testosterone reportability.1.0` = col_character(),
`Vitamin D assay date.0.0` = col_date(format = ""),
`Vitamin D assay date.1.0` = col_date(format = ""),
`Vitamin D aliquot.0.0` = col_character(),
`Vitamin D aliquot.1.0` = col_character(),
`Vitamin D correction level.0.0` = col_character(),
`Vitamin D correction level.1.0` = col_character(),
`Vitamin D correction reason.0.0` = col_character(),
`Vitamin D correction reason.1.0` = col_character(),
`Vitamin D missing reason.0.0` = col_character(),
`Vitamin D missing reason.1.0` = col_character(),
`Vitamin D reportability.0.0` = col_character(),
`Vitamin D reportability.1.0` = col_character(),
`Underlying (primary) cause of death: ICD10.0.0` = col_character(),
`Underlying (primary) cause of death: ICD10.1.0` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.0` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.1` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.2` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.3` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.4` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.5` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.6` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.7` = col_character(),
`Contributory (secondary) causes of death: ICD10.0.8` = col_logical(),
`Contributory (secondary) causes of death: ICD10.0.9` = col_logical(),
`Contributory (secondary) causes of death: ICD10.0.10` = col_logical(),
`Contributory (secondary) causes of death: ICD10.0.11` = col_logical(),
`Contributory (secondary) causes of death: ICD10.0.12` = col_logical(),
`Contributory (secondary) causes of death: ICD10.0.13` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.0` = col_character(),
`Contributory (secondary) causes of death: ICD10.1.1` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.2` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.3` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.4` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.5` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.6` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.7` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.8` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.9` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.10` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.11` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.12` = col_logical(),
`Contributory (secondary) causes of death: ICD10.1.13` = col_logical(),
`Type of cancer: ICD10.0.0` = col_character(),
`Type of cancer: ICD10.1.0` = col_character(),
`Type of cancer: ICD10.2.0` = col_character(),
`Type of cancer: ICD10.3.0` = col_character(),
`Type of cancer: ICD10.4.0` = col_character(),
`Type of cancer: ICD10.5.0` = col_character(),
`Type of cancer: ICD10.6.0` = col_logical(),
`Type of cancer: ICD10.7.0` = col_logical(),
`Type of cancer: ICD10.8.0` = col_logical(),
`Type of cancer: ICD10.9.0` = col_logical(),
`Type of cancer: ICD10.10.0` = col_logical(),
`Type of cancer: ICD10.11.0` = col_logical(),
`Type of cancer: ICD10.12.0` = col_logical(),
`Type of cancer: ICD10.13.0` = col_logical(),
`Type of cancer: ICD10.14.0` = col_logical(),
`Type of cancer: ICD10.15.0` = col_logical(),
`Type of cancer: ICD10.16.0` = col_logical(),
`Type of cancer: ICD9.0.0` = col_double(),
`Type of cancer: ICD9.1.0` = col_double(),
`Type of cancer: ICD9.2.0` = col_double(),
`Type of cancer: ICD9.3.0` = col_double(),
`Type of cancer: ICD9.4.0` = col_double(),
`Type of cancer: ICD9.5.0` = col_double(),
`Type of cancer: ICD9.6.0` = col_logical(),
`Type of cancer: ICD9.7.0` = col_logical(),
`Type of cancer: ICD9.8.0` = col_logical(),
`Type of cancer: ICD9.9.0` = col_logical(),
`Type of cancer: ICD9.10.0` = col_logical(),
`Type of cancer: ICD9.11.0` = col_logical(),
`Type of cancer: ICD9.12.0` = col_logical(),
`Type of cancer: ICD9.13.0` = col_logical(),
`Type of cancer: ICD9.14.0` = col_logical(),
`Diagnoses - main ICD10.0.0` = col_character(),
`Diagnoses - main ICD10.0.1` = col_character(),
`Diagnoses - main ICD10.0.2` = col_character(),
`Diagnoses - main ICD10.0.3` = col_character(),
`Diagnoses - main ICD10.0.4` = col_character(),
`Diagnoses - main ICD10.0.5` = col_character(),
`Diagnoses - main ICD10.0.6` = col_character(),
`Diagnoses - main ICD10.0.7` = col_character(),
`Diagnoses - main ICD10.0.8` = col_character(),
`Diagnoses - main ICD10.0.9` = col_character(),
`Diagnoses - main ICD10.0.10` = col_character(),
`Diagnoses - main ICD10.0.11` = col_character(),
`Diagnoses - main ICD10.0.12` = col_character(),
`Diagnoses - main ICD10.0.13` = col_character(),
`Diagnoses - main ICD10.0.14` = col_character(),
`Diagnoses - main ICD10.0.15` = col_character(),
`Diagnoses - main ICD10.0.16` = col_character(),
`Diagnoses - main ICD10.0.17` = col_character(),
`Diagnoses - main ICD10.0.18` = col_character(),
`Diagnoses - main ICD10.0.19` = col_character(),
`Diagnoses - main ICD10.0.20` = col_character(),
`Diagnoses - main ICD10.0.21` = col_character(),
`Diagnoses - main ICD10.0.22` = col_character(),
`Diagnoses - main ICD10.0.23` = col_character(),
`Diagnoses - main ICD10.0.24` = col_character(),
`Diagnoses - main ICD10.0.25` = col_character(),
`Diagnoses - main ICD10.0.26` = col_character(),
`Diagnoses - main ICD10.0.27` = col_character(),
`Diagnoses - main ICD10.0.28` = col_character(),
`Diagnoses - main ICD10.0.29` = col_character(),
`Diagnoses - main ICD10.0.30` = col_character(),
`Diagnoses - main ICD10.0.31` = col_character(),
`Diagnoses - main ICD10.0.32` = col_character(),
`Diagnoses - main ICD10.0.33` = col_character(),
`Diagnoses - main ICD10.0.34` = col_character(),
`Diagnoses - main ICD10.0.35` = col_character(),
`Diagnoses - main ICD10.0.36` = col_character(),
`Diagnoses - main ICD10.0.37` = col_character(),
`Diagnoses - main ICD10.0.38` = col_character(),
`Diagnoses - main ICD10.0.39` = col_character(),
`Diagnoses - main ICD10.0.40` = col_character(),
`Diagnoses - main ICD10.0.41` = col_character(),
`Diagnoses - main ICD10.0.42` = col_character(),
`Diagnoses - main ICD10.0.43` = col_character(),
`Diagnoses - main ICD10.0.44` = col_character(),
`Diagnoses - main ICD10.0.45` = col_logical(),
`Diagnoses - main ICD10.0.46` = col_logical(),
`Diagnoses - main ICD10.0.47` = col_logical(),
`Diagnoses - main ICD10.0.48` = col_logical(),
`Diagnoses - main ICD10.0.49` = col_logical(),
`Diagnoses - main ICD10.0.50` = col_logical(),
`Diagnoses - main ICD10.0.51` = col_logical(),
`Diagnoses - main ICD10.0.52` = col_logical(),
`Diagnoses - main ICD10.0.53` = col_logical(),
`Diagnoses - main ICD10.0.54` = col_logical(),
`Diagnoses - main ICD10.0.55` = col_logical(),
`Diagnoses - main ICD10.0.56` = col_logical(),
`Diagnoses - main ICD10.0.57` = col_logical(),
`Diagnoses - main ICD10.0.58` = col_logical(),
`Diagnoses - main ICD10.0.59` = col_logical(),
`Diagnoses - main ICD10.0.60` = col_logical(),
`Diagnoses - main ICD10.0.61` = col_logical(),
`Diagnoses - main ICD10.0.62` = col_logical(),
`Diagnoses - main ICD10.0.63` = col_logical(),
`Diagnoses - main ICD10.0.64` = col_logical(),
`Diagnoses - main ICD10.0.65` = col_logical(),
`Diagnoses - main ICD9.0.0` = col_character(),
`Diagnoses - main ICD9.0.1` = col_character(),
`Diagnoses - main ICD9.0.2` = col_character(),
`Diagnoses - main ICD9.0.3` = col_character(),
`Diagnoses - main ICD9.0.4` = col_character(),
`Diagnoses - main ICD9.0.5` = col_character(),
`Diagnoses - main ICD9.0.6` = col_character(),
`Diagnoses - main ICD9.0.7` = col_double(),
`Diagnoses - main ICD9.0.8` = col_character(),
`Diagnoses - main ICD9.0.9` = col_character(),
`Diagnoses - main ICD9.0.10` = col_double(),
`Diagnoses - main ICD9.0.11` = col_logical(),
`Diagnoses - main ICD9.0.12` = col_logical(),
`Diagnoses - main ICD9.0.13` = col_logical(),
`Diagnoses - main ICD9.0.14` = col_logical(),
`Diagnoses - main ICD9.0.15` = col_logical(),
`Diagnoses - main ICD9.0.16` = col_logical(),
`Diagnoses - main ICD9.0.17` = col_logical(),
`Diagnoses - main ICD9.0.18` = col_logical(),
`Diagnoses - main ICD9.0.19` = col_logical(),
`Diagnoses - main ICD9.0.20` = col_logical(),
`Diagnoses - main ICD9.0.21` = col_logical(),
`Diagnoses - main ICD9.0.22` = col_logical(),
`Diagnoses - main ICD9.0.23` = col_logical(),
`Diagnoses - main ICD9.0.24` = col_logical(),
`Diagnoses - main ICD9.0.25` = col_logical(),
`Diagnoses - main ICD9.0.26` = col_logical(),
`Diagnoses - main ICD9.0.27` = col_logical(),
`Diagnoses - secondary ICD10.0.0` = col_character(),
`Diagnoses - secondary ICD10.0.1` = col_character(),
`Diagnoses - secondary ICD10.0.2` = col_character(),
`Diagnoses - secondary ICD10.0.3` = col_character(),
`Diagnoses - secondary ICD10.0.4` = col_character(),
`Diagnoses - secondary ICD10.0.5` = col_character(),
`Diagnoses - secondary ICD10.0.6` = col_character(),
`Diagnoses - secondary ICD10.0.7` = col_character(),
`Diagnoses - secondary ICD10.0.8` = col_character(),
`Diagnoses - secondary ICD10.0.9` = col_character(),
`Diagnoses - secondary ICD10.0.10` = col_character(),
`Diagnoses - secondary ICD10.0.11` = col_character(),
`Diagnoses - secondary ICD10.0.12` = col_character(),
`Diagnoses - secondary ICD10.0.13` = col_character(),
`Diagnoses - secondary ICD10.0.14` = col_character(),
`Diagnoses - secondary ICD10.0.15` = col_character(),
`Diagnoses - secondary ICD10.0.16` = col_character(),
`Diagnoses - secondary ICD10.0.17` = col_character(),
`Diagnoses - secondary ICD10.0.18` = col_character(),
`Diagnoses - secondary ICD10.0.19` = col_character(),
`Diagnoses - secondary ICD10.0.20` = col_character(),
`Diagnoses - secondary ICD10.0.21` = col_character(),
`Diagnoses - secondary ICD10.0.22` = col_character(),
`Diagnoses - secondary ICD10.0.23` = col_character(),
`Diagnoses - secondary ICD10.0.24` = col_character(),
`Diagnoses - secondary ICD10.0.25` = col_character(),
`Diagnoses - secondary ICD10.0.26` = col_character(),
`Diagnoses - secondary ICD10.0.27` = col_character(),
`Diagnoses - secondary ICD10.0.28` = col_character(),
`Diagnoses - secondary ICD10.0.29` = col_character(),
`Diagnoses - secondary ICD10.0.30` = col_character(),
`Diagnoses - secondary ICD10.0.31` = col_character(),
`Diagnoses - secondary ICD10.0.32` = col_character(),
`Diagnoses - secondary ICD10.0.33` = col_character(),
`Diagnoses - secondary ICD10.0.34` = col_character(),
`Diagnoses - secondary ICD10.0.35` = col_character(),
`Diagnoses - secondary ICD10.0.36` = col_character(),
`Diagnoses - secondary ICD10.0.37` = col_character(),
`Diagnoses - secondary ICD10.0.38` = col_character(),
`Diagnoses - secondary ICD10.0.39` = col_character(),
`Diagnoses - secondary ICD10.0.40` = col_character(),
`Diagnoses - secondary ICD10.0.41` = col_character(),
`Diagnoses - secondary ICD10.0.42` = col_character(),
`Diagnoses - secondary ICD10.0.43` = col_character(),
`Diagnoses - secondary ICD10.0.44` = col_character(),
`Diagnoses - secondary ICD10.0.45` = col_character(),
`Diagnoses - secondary ICD10.0.46` = col_character(),
`Diagnoses - secondary ICD10.0.47` = col_character(),
`Diagnoses - secondary ICD10.0.48` = col_character(),
`Diagnoses - secondary ICD10.0.49` = col_character(),
`Diagnoses - secondary ICD10.0.50` = col_character(),
`Diagnoses - secondary ICD10.0.51` = col_character(),
`Diagnoses - secondary ICD10.0.52` = col_character(),
`Diagnoses - secondary ICD10.0.53` = col_character(),
`Diagnoses - secondary ICD10.0.54` = col_character(),
`Diagnoses - secondary ICD10.0.55` = col_character(),
`Diagnoses - secondary ICD10.0.56` = col_character(),
`Diagnoses - secondary ICD10.0.57` = col_character(),
`Diagnoses - secondary ICD10.0.58` = col_character(),
`Diagnoses - secondary ICD10.0.59` = col_character(),
`Diagnoses - secondary ICD10.0.60` = col_character(),
`Diagnoses - secondary ICD10.0.61` = col_character(),
`Diagnoses - secondary ICD10.0.62` = col_character(),
`Diagnoses - secondary ICD10.0.63` = col_character(),
`Diagnoses - secondary ICD10.0.64` = col_character(),
`Diagnoses - secondary ICD10.0.65` = col_character(),
`Diagnoses - secondary ICD10.0.66` = col_character(),
`Diagnoses - secondary ICD10.0.67` = col_character(),
`Diagnoses - secondary ICD10.0.68` = col_character(),
`Diagnoses - secondary ICD10.0.69` = col_character(),
`Diagnoses - secondary ICD10.0.70` = col_character(),
`Diagnoses - secondary ICD10.0.71` = col_character(),
`Diagnoses - secondary ICD10.0.72` = col_character(),
`Diagnoses - secondary ICD10.0.73` = col_character(),
`Diagnoses - secondary ICD10.0.74` = col_character(),
`Diagnoses - secondary ICD10.0.75` = col_character(),
`Diagnoses - secondary ICD10.0.76` = col_character(),
`Diagnoses - secondary ICD10.0.77` = col_character(),
`Diagnoses - secondary ICD10.0.78` = col_character(),
`Diagnoses - secondary ICD10.0.79` = col_character(),
`Diagnoses - secondary ICD10.0.80` = col_character(),
`Diagnoses - secondary ICD10.0.81` = col_character(),
`Diagnoses - secondary ICD10.0.82` = col_character(),
`Diagnoses - secondary ICD10.0.83` = col_character(),
`Diagnoses - secondary ICD10.0.84` = col_character(),
`Diagnoses - secondary ICD10.0.85` = col_character(),
`Diagnoses - secondary ICD10.0.86` = col_character(),
`Diagnoses - secondary ICD10.0.87` = col_character(),
`Diagnoses - secondary ICD10.0.88` = col_character(),
`Diagnoses - secondary ICD10.0.89` = col_character(),
`Diagnoses - secondary ICD10.0.90` = col_character(),
`Diagnoses - secondary ICD10.0.91` = col_character(),
`Diagnoses - secondary ICD10.0.92` = col_character(),
`Diagnoses - secondary ICD10.0.93` = col_character(),
`Diagnoses - secondary ICD10.0.94` = col_character(),
`Diagnoses - secondary ICD10.0.95` = col_character(),
`Diagnoses - secondary ICD10.0.96` = col_character(),
`Diagnoses - secondary ICD10.0.97` = col_character(),
`Diagnoses - secondary ICD10.0.98` = col_character(),
`Diagnoses - secondary ICD10.0.99` = col_character(),
`Diagnoses - secondary ICD10.0.100` = col_character(),
`Diagnoses - secondary ICD10.0.101` = col_character(),
`Diagnoses - secondary ICD10.0.102` = col_character(),
`Diagnoses - secondary ICD10.0.103` = col_character(),
`Diagnoses - secondary ICD10.0.104` = col_logical(),
`Diagnoses - secondary ICD10.0.105` = col_logical(),
`Diagnoses - secondary ICD10.0.106` = col_logical(),
`Diagnoses - secondary ICD10.0.107` = col_logical(),
`Diagnoses - secondary ICD10.0.108` = col_logical(),
`Diagnoses - secondary ICD10.0.109` = col_logical(),
`Diagnoses - secondary ICD10.0.110` = col_logical(),
`Diagnoses - secondary ICD10.0.111` = col_logical(),
`Diagnoses - secondary ICD10.0.112` = col_logical(),
`Diagnoses - secondary ICD10.0.113` = col_logical(),
`Diagnoses - secondary ICD10.0.114` = col_logical(),
`Diagnoses - secondary ICD10.0.115` = col_logical(),
`Diagnoses - secondary ICD10.0.116` = col_logical(),
`Diagnoses - secondary ICD10.0.117` = col_logical(),
`Diagnoses - secondary ICD10.0.118` = col_logical(),
`Diagnoses - secondary ICD10.0.119` = col_logical(),
`Diagnoses - secondary ICD10.0.120` = col_logical(),
`Diagnoses - secondary ICD10.0.121` = col_logical(),
`Diagnoses - secondary ICD10.0.122` = col_logical(),
`Diagnoses - secondary ICD10.0.123` = col_logical(),
`Diagnoses - secondary ICD10.0.124` = col_logical(),
`Diagnoses - secondary ICD10.0.125` = col_logical(),
`Diagnoses - secondary ICD10.0.126` = col_logical(),
`Diagnoses - secondary ICD10.0.127` = col_logical(),
`Diagnoses - secondary ICD10.0.128` = col_logical(),
`Diagnoses - secondary ICD10.0.129` = col_logical(),
`Diagnoses - secondary ICD10.0.130` = col_logical(),
`Diagnoses - secondary ICD10.0.131` = col_logical(),
`Diagnoses - secondary ICD10.0.132` = col_logical(),
`Diagnoses - secondary ICD10.0.133` = col_logical(),
`Diagnoses - secondary ICD10.0.134` = col_logical(),
`Diagnoses - secondary ICD10.0.135` = col_logical(),
`Diagnoses - secondary ICD10.0.136` = col_logical(),
`Diagnoses - secondary ICD10.0.137` = col_logical(),
`Diagnoses - secondary ICD10.0.138` = col_logical(),
`Diagnoses - secondary ICD10.0.139` = col_logical(),
`Diagnoses - secondary ICD10.0.140` = col_logical(),
`Diagnoses - secondary ICD10.0.141` = col_logical(),
`Diagnoses - secondary ICD10.0.142` = col_logical(),
`Diagnoses - secondary ICD10.0.143` = col_logical(),
`Diagnoses - secondary ICD10.0.144` = col_logical(),
`Diagnoses - secondary ICD10.0.145` = col_logical(),
`Diagnoses - secondary ICD10.0.146` = col_logical(),
`Diagnoses - secondary ICD10.0.147` = col_logical(),
`Diagnoses - secondary ICD10.0.148` = col_logical(),
`Diagnoses - secondary ICD10.0.149` = col_logical(),
`Diagnoses - secondary ICD10.0.150` = col_logical(),
`Diagnoses - secondary ICD10.0.151` = col_logical(),
`Diagnoses - secondary ICD10.0.152` = col_logical(),
`Diagnoses - secondary ICD10.0.153` = col_logical(),
`Diagnoses - secondary ICD10.0.154` = col_logical(),
`Diagnoses - secondary ICD10.0.155` = col_logical(),
`Diagnoses - secondary ICD10.0.156` = col_logical(),
`Diagnoses - secondary ICD10.0.157` = col_logical(),
`Diagnoses - secondary ICD10.0.158` = col_logical(),
`Diagnoses - secondary ICD10.0.159` = col_logical(),
`Diagnoses - secondary ICD10.0.160` = col_logical(),
`Diagnoses - secondary ICD10.0.161` = col_logical(),
`Diagnoses - secondary ICD10.0.162` = col_logical(),
`Diagnoses - secondary ICD10.0.163` = col_logical(),
`Diagnoses - secondary ICD10.0.164` = col_logical(),
`Diagnoses - secondary ICD10.0.165` = col_logical(),
`Diagnoses - secondary ICD10.0.166` = col_logical(),
`Diagnoses - secondary ICD10.0.167` = col_logical(),
`Diagnoses - secondary ICD10.0.168` = col_logical(),
`Diagnoses - secondary ICD10.0.169` = col_logical(),
`Diagnoses - secondary ICD10.0.170` = col_logical(),
`Diagnoses - secondary ICD10.0.171` = col_logical(),
`Diagnoses - secondary ICD10.0.172` = col_logical(),
`Diagnoses - secondary ICD10.0.173` = col_logical(),
`Diagnoses - secondary ICD10.0.174` = col_logical(),
`Diagnoses - secondary ICD10.0.175` = col_logical(),
`Diagnoses - secondary ICD10.0.176` = col_logical(),
`Diagnoses - secondary ICD10.0.177` = col_logical(),
`Diagnoses - secondary ICD10.0.178` = col_logical(),
`Diagnoses - secondary ICD10.0.179` = col_logical(),
`Diagnoses - secondary ICD10.0.180` = col_logical(),
`Diagnoses - secondary ICD10.0.181` = col_logical(),
`Diagnoses - secondary ICD10.0.182` = col_logical(),
`Diagnoses - secondary ICD10.0.183` = col_logical(),
`Diagnoses - secondary ICD9.0.0` = col_character(),
`Diagnoses - secondary ICD9.0.1` = col_character(),
`Diagnoses - secondary ICD9.0.2` = col_character(),
`Diagnoses - secondary ICD9.0.3` = col_character(),
`Diagnoses - secondary ICD9.0.4` = col_character(),
`Diagnoses - secondary ICD9.0.5` = col_character(),
`Diagnoses - secondary ICD9.0.6` = col_character(),
`Diagnoses - secondary ICD9.0.7` = col_character(),
`Diagnoses - secondary ICD9.0.8` = col_character(),
`Diagnoses - secondary ICD9.0.9` = col_character(),
`Diagnoses - secondary ICD9.0.10` = col_character(),
`Diagnoses - secondary ICD9.0.11` = col_character(),
`Diagnoses - secondary ICD9.0.12` = col_logical(),
`Diagnoses - secondary ICD9.0.13` = col_logical(),
`Diagnoses - secondary ICD9.0.14` = col_logical(),
`Diagnoses - secondary ICD9.0.15` = col_logical(),
`Diagnoses - secondary ICD9.0.16` = col_logical(),
`Diagnoses - secondary ICD9.0.17` = col_logical(),
`Diagnoses - secondary ICD9.0.18` = col_logical(),
`Diagnoses - secondary ICD9.0.19` = col_logical(),
`Diagnoses - secondary ICD9.0.20` = col_logical(),
`Diagnoses - secondary ICD9.0.21` = col_logical(),
`Diagnoses - secondary ICD9.0.22` = col_logical(),
`Diagnoses - secondary ICD9.0.23` = col_logical(),
`Diagnoses - secondary ICD9.0.24` = col_logical(),
`Diagnoses - secondary ICD9.0.25` = col_logical(),
`Diagnoses - secondary ICD9.0.26` = col_logical(),
`Diagnoses - secondary ICD9.0.27` = col_logical(),
`Diagnoses - secondary ICD9.0.28` = col_logical(),
`Diagnoses - secondary ICD9.0.29` = col_logical(),
`Coffee consumed.0.0` = col_character(),
`Coffee consumed.1.0` = col_character(),
`Coffee consumed.2.0` = col_character(),
`Coffee consumed.3.0` = col_character(),
`Coffee consumed.4.0` = col_character()))


print("selected cols")
ms_cases = new_bd %>% filter_at(vars(contains('ICD10')),any_vars(.=="G35")) %>% mutate('MS_status' = 1)
non_ms_cases = new_bd %>% filter(!EID %in% ms_cases$EID)
print("got ICD MS cases")
icd9_ms_cases1 = non_ms_cases %>% filter_at(vars(contains('ICD9')),any_vars(.=="3409")) %>% mutate('MS_status' = 1)
non_ms_cases = non_ms_cases %>% filter(!EID %in% icd9_ms_cases1$EID)
icd9_ms_cases2 = non_ms_cases %>% filter_at(vars(contains('ICD9')),any_vars(.=="340")) %>% mutate('MS_status' = 1)
non_ms_cases = non_ms_cases %>% filter(!EID %in% icd9_ms_cases2$EID) %>% mutate('MS_status' = 0)
print("got ICD MS cases")
self_ms = non_ms_cases %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1261")) %>% mutate('MS_status' = 1)
non_ms_cases = non_ms_cases %>% filter(!EID %in% self_ms$EID) %>% mutate('MS_status' = 0)
new_bd = bind_rows(ms_cases,non_ms_cases,icd9_ms_cases1,icd9_ms_cases2,self_ms)
new_bd$MS_status = factor(new_bd$MS_status)
print("converted MS status")
dim(new_bd)

self_ibd = new_bd %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1461"|.=="1462"|.=="1463")) %>% mutate('IBD_status' = 1)
no_ibd = new_bd %>% filter(!EID %in% self_ibd$EID) %>% mutate('IBD_status' = 0)
new_bd = bind_rows(self_ibd,no_ibd)
dim(new_bd)
self_ra = new_bd %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1464")) %>% mutate('RA_status' = 1)
no_ra = new_bd %>% filter(!EID %in% self_ra$EID) %>% mutate('RA_status' = 0)
new_bd = bind_rows(self_ra,no_ra)
dim(new_bd)
self_pbc = new_bd %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1506")) %>% mutate('PBC_status' = 1)
no_pbc = new_bd %>% filter(!EID %in% self_pbc$EID) %>% mutate('PBC_status' = 0)
new_bd = bind_rows(self_pbc,no_pbc)
dim(new_bd)
self_sle = new_bd %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1381")) %>% mutate('SLE_status' = 1)
no_sle = new_bd %>% filter(!EID %in% self_sle$EID) %>% mutate('SLE_status' = 0)
new_bd = bind_rows(self_sle,no_sle)
self_sjog = new_bd %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1382")) %>% mutate('SJOG_status' = 1)
no_sjog = new_bd %>% filter(!EID %in% self_sjog$EID) %>% mutate('SJOG_status' = 0)
new_bd = bind_rows(self_sjog,no_sjog)
dim(new_bd)

PD_cases = new_bd %>% filter_at(vars(contains('iagnoses')),any_vars(.=="G20")) %>% mutate('PD_status' = 1)
non_PD_cases = new_bd %>% filter(!EID %in% PD_cases$EID)
print("got ICD PD cases")
self_PD = non_PD_cases %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1262")) %>% mutate('PD_status' = 1)
non_PD_cases = non_PD_cases %>% filter(!EID %in% self_PD$EID) %>% mutate('PD_status' = 0)
new_bd = bind_rows(PD_cases,non_PD_cases,self_PD)
new_bd$PD_status = factor(new_bd$PD_status)
print("converted PD status")
dim(new_bd)

dm_cases = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="E100" | .=="E101" | .=="E102" | .=="E103"| .=="E104"| .=="E105"| .=="E106"| .=="E107"| .=="E108"| .=="E109"| .=="E110"| .=="E111"| .=="E112"| .=="E113"| .=="E114"| .=="E115"| .=="E116"| .=="E117"| .=="E118"| .=="E119"| .=="E120"))
dm_cases = dm_cases %>% mutate("DM_status"=1)
self_dm = new_bd %>% filter(!(EID %in% dm_cases$EID)) %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1220")) %>% mutate('DM_status' = 1)
non_dm_cases = new_bd %>% filter(!(EID %in% dm_cases$EID)) %>% filter(!(EID %in% self_dm$EID))%>% mutate("DM_status"=0)
new_bd = bind_rows(dm_cases,non_dm_cases,self_dm)


print("converted DM status")
dim(new_bd)
dep = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="F320" | .=="F321" | .=="F322" | .=="F323" | .=="F328" | .=="F329" | .=="F330" | .=="F331" | .=="F332" |  .=="F333" | .=="F334" | .=="F338" | .=="F339") ) %>% mutate("Depression"=1)
self_dep = new_bd %>% filter(!(EID %in% dep$EID)) %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1286")) %>% mutate('Depression' = 1)
no_dep = new_bd %>% filter(!(EID %in% dep$EID))%>% filter(!(EID %in% self_dep$EID)) %>% mutate("Depression"=0)
new_bd = bind_rows(dep,no_dep,self_dep)

print("converted depression status")
dim(new_bd)

anx = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="F411" | .=="F412" | .=="F413" | .=="F418" | .=="F419") ) %>% mutate("Anxiety"=1)
self_anx = new_bd %>% filter(!(EID %in% anx$EID)) %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1287")) %>% mutate('Anxiety' = 1)
no_anx = new_bd %>% filter(!(EID %in% anx$EID))%>% filter(!(EID %in% self_anx$EID)) %>% mutate("Anxiety"=0)
new_bd = bind_rows(anx,no_anx,self_anx)
print("converted anxiety status")
dim(new_bd)

Epilepsy_cases = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="G400" | .=="G401" | .=="G402" | .=="G403" | .=="G404" | .=="G405" | .=="G406" | .=="G407" | .=="G408" | .=="G409" ) ) %>% mutate("Epilepsy_status"=1)
print("got ICD Epilepsy cases")
self_Epilepsy = new_bd %>% filter(!EID %in% Epilepsy_cases$EID) %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1264")) %>% mutate('Epilepsy_status' = 1)
non_Epilepsy_cases = new_bd %>% filter(!EID %in% self_Epilepsy$EID) %>% filter(!EID %in% Epilepsy_cases$EID) %>% mutate('Epilepsy_status' = 0)
new_bd = bind_rows(Epilepsy_cases,non_Epilepsy_cases,self_Epilepsy)
new_bd$Epilepsy_status = factor(new_bd$Epilepsy_status)
print("converted Epilepsy status")
dim(new_bd)
migraine = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="G430" | .=="G431" ) ) %>% mutate("Migraine"=1)
self_migraine = new_bd %>%filter(!(EID %in% migraine$EID)) %>%
  filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1265")) %>% mutate('Migraine' = 1)
no_migraine = new_bd %>% filter(!(EID %in% migraine$EID))%>% 
  filter(!(EID %in% self_migraine$EID))%>%
  mutate("Migraine"=0)
new_bd = bind_rows(migraine,no_migraine,self_migraine)
print("converted migraine status")
dim(new_bd)
pd_fhx = new_bd %>% filter_at(vars(contains("Illnesses of")),any_vars(.=="Parkinson's disease")) %>% mutate("PD_FHx"=1)
no_pd_fhx = new_bd %>% filter(!(EID %in% pd_fhx$EID)) %>% mutate("PD_FHx"=0)
new_bd = bind_rows(pd_fhx,no_pd_fhx)
print("converted pd fhx status")
dim(new_bd)
depression_fhx = new_bd %>% filter_at(vars(contains("Illnesses of")),any_vars(.=="Severe depression")) %>% mutate("Depression_FHx"=1)
no_depression_fhx = new_bd %>% filter(!(EID %in% depression_fhx$EID)) %>% mutate("Depression_FHx"=0)
new_bd = bind_rows(depression_fhx,no_depression_fhx)
print("converted depression fhx status")
dim(new_bd)
dementia_fhx = new_bd %>% filter_at(vars(contains("Illnesses of")),any_vars(.=="Alzheimer's disease/dementia")) %>% mutate("Dementia_FHx"=1)
no_dementia_fhx = new_bd %>% filter(!(EID %in% dementia_fhx$EID)) %>% mutate("Dementia_FHx"=0)
new_bd = bind_rows(dementia_fhx,no_dementia_fhx)
print("converted dementia fhx status")
dim(new_bd)
Diabetes_fhx = new_bd %>% filter_at(vars(contains("Illnesses of")),any_vars(.=="Diabetes")) %>% mutate("Diabetes_FHx"=1)
no_Diabetes_fhx = new_bd %>% filter(!(EID %in% Diabetes_fhx$EID)) %>% mutate("Diabetes_FHx"=0)
new_bd = bind_rows(Diabetes_fhx,no_Diabetes_fhx)
print("converted dm fhx status")
dim(new_bd)
Stroke_fhx = new_bd %>% filter_at(vars(contains("Illnesses of")),any_vars(.=="Stroke")) %>% mutate("Stroke_FHx"=1)
no_Stroke_fhx = new_bd %>% filter(!(EID %in% Stroke_fhx$EID)) %>% mutate("Stroke_FHx"=0)
new_bd = bind_rows(Stroke_fhx,no_Stroke_fhx)
print("converted stroke fhx status")
dim(new_bd)
melanoma_cases = new_bd %>% filter_at(vars(contains("Type of cancer: ICD10")),any_vars(.=="C430" | .=="C431" | .=="C432" | .=="C433"| .=="C434"| .=="C435"| .=="C436"| .=="C437"| .=="C438"| .=="C439")) %>% mutate("melanoma_status" = 1)
non_melanoma_cases = new_bd %>% filter(!EID %in% melanoma_cases$EID) %>%  mutate("melanoma_status" = 0)
new_bd = bind_rows(melanoma_cases,non_melanoma_cases)
new_bd$melanoma_status = factor(new_bd$melanoma_status)
print("converted melanoma status")
dim(new_bd)
pest = new_bd %>% filter_at(vars(contains("Worked with pesticides")),any_vars(.=="Sometimes")) %>% mutate("Pesticides"=1)
pest_oft = new_bd %>% filter(!(EID %in% pest$EID)) %>% filter_at(vars(contains("Worked with pesticides")),any_vars(.=="Often")) %>% mutate("Pesticides"=1)
pest_never = new_bd %>% filter(!(EID %in% pest$EID)) %>% filter(!(EID %in% pest_oft$EID)) %>% filter_at(vars(contains("Worked with pesticides")),any_vars(.=="Rarely/never")) %>% mutate("Pesticides"=0)
pest_na = new_bd %>% filter(!(EID %in% pest$EID)) %>%
  filter(!(EID %in% pest_oft$EID)) %>%
  filter(!(EID %in% pest_never$EID)) %>%
  mutate("Pesticides"=NA)
new_bd = bind_rows(pest,pest_oft,pest_never,pest_na)
print("converted pesticide status")
dim(new_bd)
im = new_bd %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1567")) %>% mutate('IM_status' = 1)
non_im = new_bd %>% filter(!EID %in% im$EID) %>% mutate('IM_status' = 0)
new_bd = bind_rows(im,non_im)
dim(new_bd)

Constipation_cases = new_bd %>% filter_at(vars(contains('iagnoses')),any_vars(.=="K590")) %>% mutate('Constipation_status' = 1)
non_Constipation_cases = new_bd %>% filter(!EID %in% Constipation_cases$EID)
print("got ICD Constipation cases")
self_Constipation = non_Constipation_cases %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1599")) %>% mutate('Constipation_status' = 1)
non_Constipation_cases = non_Constipation_cases %>% filter(!EID %in% self_Constipation$EID) %>% mutate('Constipation_status' = 0)
new_bd = bind_rows(Constipation_cases,non_Constipation_cases,self_Constipation)
new_bd$Constipation_status = factor(new_bd$Constipation_status)
print("converted Constipation status")
dim(new_bd)
Head_injury_cases = new_bd %>% filter_at(vars(contains('iagnoses')),any_vars(.=="S060" | .=="S061" | .=="S062" | .=="S063" | .=="S064" | .=="S065"| .=="S066"| .=="S068"| .=="S069")) %>% mutate('Head_injury_status' = 1)
non_Head_injury_cases = new_bd %>% filter(!EID %in% Head_injury_cases$EID)
print("got ICD intracranial_injury cases")
Head_injury_cases2 = non_Head_injury_cases %>% filter_at(vars(contains('iagnoses')),any_vars(.=="S020" | .=="S021" | .=="S022" | .=="S023" | .=="S024" | .=="S026"| .=="S027"| .=="S028"| .=="S029")) %>% mutate('Head_injury_status' = 1)
non_Head_injury_cases = non_Head_injury_cases %>% filter(!EID %in% Head_injury_cases2$EID)
print("got ICD skull_fracture cases")
self_Head_injury = non_Head_injury_cases %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1266")) %>% mutate('Head_injury_status' = 1)
non_Head_injury_cases = non_Head_injury_cases %>% filter(!EID %in% self_Head_injury$EID) %>% mutate('Head_injury_status' = 0)
new_bd = bind_rows(Head_injury_cases,non_Head_injury_cases,Head_injury_cases2,self_Head_injury)
new_bd$Head_injury_status = factor(new_bd$Head_injury_status)
print("converted Head_injury status")
dim(new_bd)

Gastric_ulcer_cases = new_bd %>% filter_at(vars(contains('iagnoses')),any_vars(.=="K250"| .=="K251"| .=="K252"| .=="K253"| .=="K254"| .=="K255"| .=="K256"| .=="K257"| .=="K259")) %>% mutate('Gastric_ulcer_status' = 1)
non_Gastric_ulcer_cases = new_bd %>% filter(!EID %in% Gastric_ulcer_cases$EID)
print("got ICD Gastric_ulcer cases")
self_Gastric_ulcer = non_Gastric_ulcer_cases %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1142")) %>% mutate('Gastric_ulcer_status' = 1)
non_Gastric_ulcer_cases = non_Gastric_ulcer_cases %>% filter(!EID %in% self_Gastric_ulcer$EID) %>% mutate('Gastric_ulcer_status' = 0)
new_bd = bind_rows(Gastric_ulcer_cases,non_Gastric_ulcer_cases,self_Gastric_ulcer)
new_bd$Gastric_ulcer_status = factor(new_bd$Gastric_ulcer_status)
print("converted Gastric_ulcer status")
dim(new_bd)

Anosmia = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="R430") ) %>% mutate("Anosmia"=1)
no_Anosmia = new_bd %>% filter(!(EID %in% Anosmia$EID))%>% mutate("Anosmia"=0)
new_bd = bind_rows(Anosmia,no_Anosmia)
print("converted Anosmia status")
dim(new_bd)
Erectile_dysfunction_cases = new_bd %>% filter_at(vars(contains('iagnoses')),any_vars(.=="N484")) %>% mutate('Erectile_dysfunction_status' = 1)
non_Erectile_dysfunction_cases = new_bd %>% filter(!EID %in% Erectile_dysfunction_cases$EID)
print("got ICD Erectile_dysfunction cases")
self_Erectile_dysfunction = non_Erectile_dysfunction_cases %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1518")) %>% mutate('Erectile_dysfunction_status' = 1)
non_Erectile_dysfunction_cases = non_Erectile_dysfunction_cases %>% filter(!EID %in% self_Erectile_dysfunction$EID) %>% mutate('Erectile_dysfunction_status' = 0)
new_bd = bind_rows(Erectile_dysfunction_cases,non_Erectile_dysfunction_cases,self_Erectile_dysfunction)
new_bd$Erectile_dysfunction_status = factor(new_bd$Erectile_dysfunction_status)
print("converted Erectile_dysfunction status")
dim(new_bd)
Hypertension_cases = new_bd %>% filter_at(vars(contains('iagnoses')),any_vars(.=="I10")) %>% mutate('Hypertension_status' = 1)
non_Hypertension_cases = new_bd %>% filter(!EID %in% Hypertension_cases$EID)
print("got ICD Hypertension cases")
self_Hypertension = non_Hypertension_cases %>% filter_at(vars(contains('Non-cancer illness code, self-reported')),any_vars(.=="1072"| .=="1065")) %>% mutate('Hypertension_status' = 1)
non_Hypertension_cases = non_Hypertension_cases %>% filter(!EID %in% self_Hypertension$EID) %>% mutate('Hypertension_status' = 0)
new_bd = bind_rows(Hypertension_cases,non_Hypertension_cases,self_Hypertension)
new_bd$Hypertension_status = factor(new_bd$Hypertension_status)
print("converted Hypertension status")
dim(new_bd)
Shoulder_pain = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="M2551") ) %>% mutate("Shoulder_pain"=1)
no_Shoulder_pain = new_bd %>% filter(!(EID %in% Shoulder_pain$EID))%>% mutate("Shoulder_pain"=0)
new_bd = bind_rows(Shoulder_pain,no_Shoulder_pain)
print("converted Shoulder_pain status")
dim(new_bd)
Shoulder_stiffness = new_bd %>% filter_at(vars(contains("iagnoses")),any_vars(.=="M2561") ) %>% mutate("Shoulder_stiffness"=1)
no_Shoulder_stiffness = new_bd %>% filter(!(EID %in% Shoulder_stiffness$EID))%>% mutate("Shoulder_stiffness"=0)
new_bd = bind_rows(Shoulder_stiffness,no_Shoulder_stiffness)
print("converted Shoulder_stiffness status")
dim(new_bd)
print("got all diagnoses")

#get age at diagnosis
print("starting to get age at diagnosis")
new_df = new_bd %>% filter(MS_status==1) %>% select(contains("on-cancer"),EID)
age_at_ms_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
row = new_df[i,]
c=colnames(row)
t=t(row)
t=data.frame(cbind(t,c))
colnames(t)=c("a","b")
ms=t %>% filter(a==1261)
if (nrow(ms)==0){
next
} else {
s=str_split(ms$b,pattern="reported.")[[1]][2]
f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
if(is.na(row[[f]])){
  next
} else {
age = row[[f]]
age_at_ms_diagnosis <<- c(age_at_ms_diagnosis,age)
t=t %>% filter(b=="EID")
eid=as.numeric(as.character(t$a))
eids <<- c(eids,eid)
}
}
}
new_df = data.frame(cbind(eids,age_at_ms_diagnosis))
colnames(new_df)=c("EID","age_at_ms_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")




# get age at dm diagnosis

#get age at diagnosis
print("starting to get age at diagnosis")
new_df = new_bd %>% filter(DM_status==1) %>% select(contains("on-cancer"),EID)
age_at_dm_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
row = new_df[i,]
c=colnames(row)
t=t(row)
t=data.frame(cbind(t,c))
colnames(t)=c("a","b")
dm=t %>% filter(a==1220)
if (nrow(dm)==0){
next
} else {
s=str_split(dm$b,pattern="reported.")[[1]][2]
f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
if(is.na(row[[f]])){
  next
} else {
age = row[[f]]
age_at_dm_diagnosis <<- c(age_at_dm_diagnosis,age)
t=t %>% filter(b=="EID")
eid=as.numeric(as.character(t$a))
eids <<- c(eids,eid)
}
}
}
new_df = data.frame(cbind(eids,age_at_dm_diagnosis))
colnames(new_df)=c("EID","age_at_dm_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")



new_df = new_bd %>% filter(Constipation_status ==1) %>% select(contains("on-cancer"),EID)
age_at_constipation_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1599)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_constipation_diagnosis <<- c(age_at_constipation_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_constipation_diagnosis))
colnames(new_df)=c("EID","age_at_constipation_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got constipation age of diagnosis")

new_df = new_bd %>% filter(Depression==1) %>% select(contains("on-cancer"),EID)
age_at_depression_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1286)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_depression_diagnosis <<- c(age_at_depression_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_depression_diagnosis))
colnames(new_df)=c("EID","age_at_depression_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got depression age of diagnosis")

new_df = new_bd %>% filter(Anxiety ==1) %>% select(contains("on-cancer"),EID)
age_at_anxiety_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1287)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_anxiety_diagnosis <<- c(age_at_anxiety_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_anxiety_diagnosis))
colnames(new_df)=c("EID","age_at_anxiety_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got anxiety age of diagnosis")

new_df = new_bd %>% filter(Head_injury_status ==1) %>% select(contains("on-cancer"),EID)
age_at_head_injury_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1266)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_head_injury_diagnosis <<- c(age_at_head_injury_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_head_injury_diagnosis))
colnames(new_df)=c("EID","age_at_head_injury_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got head_injury age of diagnosis")


new_df = new_bd %>% filter(Hypertension_status ==1) %>% select(contains("on-cancer"),EID)
age_at_Hypertension_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1065)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_Hypertension_diagnosis <<- c(age_at_Hypertension_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_Hypertension_diagnosis))
colnames(new_df)=c("EID","age_at_Hypertension_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got Hypertension age of diagnosis")

new_df = new_bd %>% filter(Gastric_ulcer_status ==1) %>% select(contains("on-cancer"),EID)
age_at_Gastric_ulcer_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1142)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_Gastric_ulcer_diagnosis <<- c(age_at_Gastric_ulcer_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_Gastric_ulcer_diagnosis))
colnames(new_df)=c("EID","age_at_Gastric_ulcer_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got Gastric_ulcer age of diagnosis")


new_df = new_bd %>% filter(Erectile_dysfunction_status ==1) %>% select(contains("on-cancer"),EID)
age_at_Erectile_dysfunction_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1518)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_Erectile_dysfunction_diagnosis <<- c(age_at_Erectile_dysfunction_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_Erectile_dysfunction_diagnosis))
colnames(new_df)=c("EID","age_at_Erectile_dysfunction_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got Erectile_dysfunction age of diagnosis")

# migraine
new_df = new_bd %>% filter(Migraine ==1) %>% select(contains("on-cancer"),EID)
age_at_migraine_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1265)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_migraine_diagnosis <<- c(age_at_migraine_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_migraine_diagnosis))
colnames(new_df)=c("EID","age_at_migraine_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got migraine age of diagnosis")

#epilepsy
new_df = new_bd %>% filter(Epilepsy_status ==1) %>% select(contains("on-cancer"),EID)
age_at_epilepsy_diagnosis = c()
eids=c()
for (i in 1:nrow(new_df)){
  row = new_df[i,]
  c=colnames(row)
  t=t(row)
  t=data.frame(cbind(t,c))
  colnames(t)=c("a","b")
  ms=t %>% filter(a==1264)
  if (nrow(ms)==0){
    next
  } else {
    s=str_split(ms$b,pattern="reported.")[[1]][2]
    f=paste0("Interpolated Age of participant when non-cancer illness first diagnosed.",s)
    if(is.na(row[[f]])){
      next
    } else {
      age = row[[f]]
      age_at_epilepsy_diagnosis <<- c(age_at_epilepsy_diagnosis,age)
      t=t %>% filter(b=="EID")
      eid=as.numeric(as.character(t$a))
      eids <<- c(eids,eid)
    }
  }
}
new_df = data.frame(cbind(eids,age_at_epilepsy_diagnosis))
colnames(new_df)=c("EID","age_at_epilepsy_diagnosis")
print("converting EIDs")

new_df$EID = as.numeric(as.character(new_df$EID))
new_bd$EID = as.numeric(as.character(new_bd$EID))
print("converted EIDs")

new_bd=left_join(new_bd,new_df,by="EID")

print("got epilepsy age of diagnosis")


new_bd = new_bd %>%
select(EID,
Sex.0.0,
`Year of birth.0.0`,
`Age at recruitment.0.0`,
`Month of birth.0.0`,
`Place of birth in UK - north co-ordinate.0.0`,
`Age completed full time education.0.0`,
`Country of birth (UK/elsewhere).0.0`,
`Breastfed as a baby.0.0`,
`Comparative body size at age 10.0.0`,
`Maternal smoking around birth.0.0`,
`Age first had sexual intercourse.0.0`,
`Relative age of first facial hair.0.0`,
`Relative age voice broke.0.0`,
`Age when periods started (menarche).0.0`,
`Birth weight.0.0`,
`Smoking status.0.0`,
`Ethnic background.0.0`,
`HLA imputation values.0.0`,
`Townsend deprivation index at recruitment.0.0`,
contains("enetic"),
MS_status,
PD_status,
DM_status,
Depression,
Anxiety,
Migraine,
PD_FHx,
Dementia_FHx,
Depression_FHx,
Diabetes_FHx,
Stroke_FHx,
melanoma_status,
Pesticides,
IM_status,
contains("oking"),
contains("affeine"),
contains("diet"),
contains("supplement"),
contains("intake"),
contains("outdoors"),
`Alcohol drinker status.0.0`,
`Had menopause.0.0`,
`Falls in the last year.0.0`,
`Fractured/broken bones in last 5 years.0.0`,
`Heel bone mineral density (BMD) T-score, automated.0.0`,
`Usual walking pace.0.0`,
`Myopia diagnosis.0.0`,
contains("edication"),
Constipation_status,
Head_injury_status,
Epilepsy_status,
Gastric_ulcer_status,
Anosmia,
Erectile_dysfunction_status,
Hypertension_status,
Shoulder_pain,
Shoulder_stiffness,
age_at_ms_diagnosis,
age_at_dm_diagnosis,
age_at_constipation_diagnosis,
age_at_depression_diagnosis,
age_at_anxiety_diagnosis,
age_at_head_injury_diagnosis,
age_at_Hypertension_diagnosis,
age_at_Gastric_ulcer_diagnosis,
age_at_Erectile_dysfunction_diagnosis,
contains("oking"),
contains("affeine"),
contains("offee"),
contains("ea intake"),
contains("ea consumed"),
contains("lcohol intake frequency"),
contains("rate"),
contains("holesterol"),
contains("ody mass index"),
contains("ge completed full time education"),
contains("aytime dozing / sleeping (narcolepsy)"),
contains("air colour"),
age_at_epilepsy_diagnosis,
age_at_migraine_diagnosis)




write_tsv(new_bd,path=args[2])



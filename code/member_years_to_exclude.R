exclude <- tribble(
  ~icpsr, ~chamber, ~year,
  # No congressional service during this calendar year
  29137, "House", 2016, # John Boehner — resigned Oct. 31, 2015
  29931, "House", 2012, # David Wu — resigned Aug. 3, 2011
  21376, "House", 2013, # Bradley Byrne — entered Jan. 8, 2014
  21377, "House", 2013, # Curt Clawson — entered June 2014

  # Served fewer than 10 months after a special election
  21375, "House", 2013, # Katherine Clark — entered Dec. 2013
  21762, "House", 2018, # Mary Gay Scanlon — entered Nov. 2018
  21763, "House", 2018, # Susan Wild — entered Nov. 2018
  21986, "House", 2019, # Dan Bishop — entered Sept. 2019
  22161, "House", 2021, # Shontel Brown — entered Nov. 2021
  22167, "House", 2022, # Brad Finstad — entered Aug. 2022
  22375, "House", 2023,  # Gabe Amo — entered Nov. 2023

  # Party switches: served fewer than 10 months under this party-specific ICPSR
  14910, "Senate", 2009, # Arlen Specter (R) — Republican only through Apr. 2009
  14910, "Senate", 2010, # Arlen Specter (R) — Democrat throughout 2010
  90901, "House",  2009, # Parker Griffith (R) — became Republican in Dec. 2009
  94828, "House",  2003, # Ralph Hall (R) — did not become Republican until Jan. 2004
  91143, "House",  2019, # Justin Amash (I) — became independent in July 2019
  91143, "House",  2020, # Justin Amash (I) — became Libertarian in Apr. 2020
  91980, "House",  2019, # Jeff Van Drew (R) — became Republican in Dec. 2019
  14240, "Senate", 2001, # Jim Jeffords (R) — left the Republican Party in May 2001
  14240, "Senate", 2002, # Jim Jeffords (R) — independent throughout 2002
  94879, "House",  2000, # Matthew Martínez (R) — became Republican in July 2000
  99767, "House",  2002, # Virgil Goode (I) — affiliated with Republicans in Aug. 2002

  # Departures: served fewer than 10 months during the listed year
  20505, "House",  2017, # Tom Price (R) — left the House in Feb. 2017
  21147, "House",  2015, # Alan Nunnelee (R) — died in Feb. 2015
  21176, "House",  2017, # Mick Mulvaney (R) — left the House in Feb. 2017
  21532, "House",  2017, # Ryan Zinke (R) — left the House in Mar. 2017
  40912, "Senate", 2010, # Paul Kirk (D) — service ended in Feb. 2010
  41307, "Senate", 2013, # Jeffrey Chiesa (R) — served June–Oct. 2013
  49700, "Senate", 2017, # Jeff Sessions (R) — left the Senate in Feb. 2017

  # Arrivals: served fewer than 10 months during the listed year
  20941, "House",  2008, # Marcia Fudge (D) — entered in Nov. 2008
  21124, "House",  2016, # Colleen Hanabusa (D) — returned in Nov. 2016
  21565, "House",  2016, # James Comer (R) — entered in Nov. 2016
  21566, "House",  2016, # Dwight Evans (D) — entered in Nov. 2016
  31101, "House",  2012, # Suzan DelBene (D) — entered in Nov. 2012
  31103, "House",  2012, # Donald Payne Jr. (D) — entered in Nov. 2012
  40912, "Senate", 2009, # Paul Kirk (D) — appointed in Sept. 2009
  40915, "Senate", 2010, # Joe Manchin (D) — entered in Nov. 2010
  40916, "Senate", 2010, # Chris Coons (D) — entered in Nov. 2010
  21758, "House",  2018, # Michael Cloud (R) — entered in July 2018
  21759, "House",  2018, # Troy Balderson (R) — entered in Sept. 2018
  21760, "House",  2018, # Kevin Hern (R) — entered in Nov. 2018
  15433, "House",  2020, # Kweisi Mfume (D) — returned in May 2020
  21988, "House",  2020, # Mike Garcia (R) — entered in May 2020
  22170, "House",  2022, # Joseph Sempolinski (R) — entered in Sept. 2022
  22171, "House",  2022, # Rudy Yakym (R) — entered in Nov. 2022
  22379, "House",  2024, # Michael Rulli (R) — entered in June 2024
  22382, "House",  2024, # Erica Lee Carter (D) — entered in Nov. 2024
  22383, "House",  2024, # Tony Wied (R) — entered in Nov. 2024
  41905, "Senate", 2020,  # Mark Kelly (D) — entered in Dec. 2020
  # No service at any point during the listed calendar year
  40912, "Senate", 2010, # Paul Kirk — appointed in Sept. 2009; service ended Feb. 2010

  # Party-specific ICPSR code does not apply during this calendar year
  14910, "Senate", 2010, # Arlen Specter — switched from Republican to Democratic in April 2009;
  # Republican ICPSR 14910 should not represent his 2010 service
  14240, "Senate", 2002,  # Jim Jeffords — left the Republican Party in May 2001;
  # Republican ICPSR 14240 should not represent his 2002 service
  94828, "House",  2003,  # Ralph Hall — Democrat throughout 2003; became Republican in Jan. 2004

  # Left office before the listed year
  14101, "Senate", 2010, # Joe Biden — left the Senate in January 2009
  29736, "House",  2014, # Jo Ann Emerson — left the House in January 2013
  40500, "Senate", 2010, # Ken Salazar — left the Senate in January 2009
  21136, "House",  2018, # Mike Pompeo — left the House in January 2017
  29316, "House",  2018, # Xavier Becerra — left the House in January 2017
  21137, "House",  2022, # Cedric Richmond — left the House in January 2021
  21170, "House",  2020, # Tom Marino — left the House in January 2019
  41701, "Senate", 2022, # Kamala Harris — left the Senate in January 2021
  41904, "Senate", 2022, # Kelly Loeffler — Senate service ended in January 2021
  10573, "House",  2000, # George Brown Jr. — died in July 1999
  14469, "House",  2000, # Bob Livingston — resigned in March 1999
  14620, "House",  2002,  # Julian Dixon — died in December 2000 # ERROR IN VOTEVIEW

  # Entered Congress after the listed year began
  41112, "Senate", 2012, # Brian Schatz — appointed Dec. 2012
  # 106th Congress (1999–2000)
  10573, "House",  1999,  # George E. Brown Jr.
  29917, "House",  1999,  # Ernie Fletcher
  14469, "House",  1999,  # Bob Livingston

  # 107th Congress (2001–2002)
  14620, "House",  2001,  # Julian Dixon

  # 111th Congress (2009–2010)
  14101, "Senate", 2009,  # Joe Biden
  14101, "Senate", 2010,  # Joe Biden
  40500, "Senate", 2009,  # Ken Salazar

  # 112th Congress (2011–2012)
  41112, "Senate", 2011,  # Brian Schatz

  # 113th Congress (2013–2014)
  29736, "House",  2013,  # Jo Ann Emerson, On January 22, 2013, Emerson resigned

  # 115th Congress (2017–2018)
  29316, "House",  2017,  # Xavier Becerra
  21136, "House",  2017,  # Mike Pompeo

  # 116th Congress (2019–2020)
  21170, "House",  2019,  # Tom Marino January 23, 2019 he resigned to work in the private sector.

  # 117th Congress (2021–2022)
  41701, "Senate", 2021,  # Kamala Harris
  41904, "Senate", 2021,  # Kelly Loeffler LOST a runoff with Democrat Raphael Warnock held on January 5, 2021
  21137, "House",  2021,  # Cedric Richmond resignation became official on January 15, 2021

  # 119th Congress (2025–2026)
  41102, "Senate", 2025,  # Marco Rubio
  42304, "Senate", 2025,  # J.D. Vance
  21981, "House",  2025,   # Michael Waltz

  # Deaths, resignations, and departures
  10808, "Senate", 2010, # Ted Kennedy — died in 2009
  13047, "House",  2014, # Bill Young — died in 2013
  14031, "Senate", 2008, # Trent Lott — resigned in 2007
  14914, "Senate", 2014, # Frank Lautenberg — died in 2013
  14920, "Senate", 2014, # John Kerry — resigned in 2013 to become secretary of state
  15093, "House",  2004, # Larry Combest — resigned in 2003
  15429, "Senate", 2017, # Jon Kyl — prior service ended in 2013; returned in 2018
  15601, "House",  2006, # Christopher Cox — resigned in 2005 to chair the SEC
  15604, "House",  2008, # Paul Gillmor — died in 2007
  15633, "Senate", 2008, # Craig Thomas — died in 2007
  20105, "House",  2010, # Hilda Solis — resigned in 2009 to become secretary of labor
  20141, "House",  2008, # Jo Ann Davis — died in 2007
  20300, "House",  2014, # Jo Bonner — resigned in 2013
  20914, "House",  2016, # Aaron Schock — resigned in 2015
  20936, "House",  2012, # Christopher Lee — resigned in 2011
  21124, "House",  2015, # Colleen Hanabusa — prior service ended in 2015; returned in 2016
  21147, "House",  2016, # Alan Nunnelee — died in 2015
  29107, "House",  2006, # Duke Cunningham — resigned in 2005
  29318, "House",  2012, # Jane Harman — resigned in 2011
  29358, "House",  2008, # Marty Meehan — resigned in 2007
  29386, "House",  2006, # Rob Portman — resigned in 2005 to become U.S. trade representative
  29513, "House",  2008, # Charlie Norwood — died in 2007
  29537, "Senate", 2012, # John Ensign — resigned in 2011
  29586, "House",  2008, # Juanita Millender-McDonald — died in 2007
  29705, "House",  2010, # Ellen Tauscher — resigned in 2009 to join the State Department
  29917, "House",  2004, # Ernie Fletcher — resigned in 2003 to become governor of Kentucky
  29926, "House",  2012, # Anthony Weiner — resigned in 2011
  39316, "House",  2010, # John McHugh — resigned in 2009 to become secretary of the Army
  40105, "Senate", 2010, # Hillary Clinton — resigned in 2009 to become secretary of state
  40501, "Senate", 2010, # Mel Martinez — resigned in 2009
  41306, "Senate", 2014, # Mo Cowan — temporary appointment ended in 2013
  41307, "Senate", 2014, # Jeffrey Chiesa — temporary appointment ended in 2013
  90327, "House",  2014, # Rodney Alexander — resigned in 2013
  10713, "House",  2018, # John Conyers — resigned in 2017
  20304, "House",  2018, # Trent Franks — resigned in 2017
  20346, "House",  2018, # Tim Murphy — resigned in 2017
  20505, "House",  2018, # Tom Price — resigned in 2017 to become HHS secretary
  20949, "House",  2018, # Jason Chaffetz — resigned in 2017
  21176, "House",  2018, # Mick Mulvaney — resigned in 2017 to become OMB director
  21532, "House",  2018, # Ryan Zinke — resigned in 2017 to become interior secretary
  49700, "Senate", 2018, # Jeff Sessions — resigned in 2017 to become attorney general
  20703, "House",  2024, # Kevin McCarthy — resigned in 2023
  20941, "House",  2022, # Marcia Fudge — resigned in 2021 to become HUD secretary
  21163, "House",  2022, # Steve Stivers — resigned in 2021
  21172, "House",  2024, # David Cicilline — resigned in 2023
  21189, "House",  2020, # Sean Duffy — resigned in 2019
  21345, "House",  2020, # Chris Collins — resigned in 2019
  21367, "House",  2024, # Chris Stewart — resigned in 2023
  21747, "House",  2023, # Tom Suozzi — prior service ended in 2023; returned in 2024
  21928, "House",  2022, # Deb Haaland — resigned in 2021 to become interior secretary
  21932, "House",  2020, # Katie Hill — resigned in 2019
  21984, "House",  2022, # Ron Wright — died in 2021
  22362, "House",  2024, # George Santos — expelled in 2023
  29337, "House",  2022, # Alcee Hastings — died in 2021
  29546, "House",  2020, # Walter Jones Jr. — died in 2019
  29587, "House",  2020, # Elijah Cummings — died in 2019
  29909, "Senate", 2020, # Johnny Isakson — resigned in 2019
  49300, "Senate", 2024, # Dianne Feinstein — died in 2023
  13042, "House",  2002, # Floyd Spence — died in 2001
  14039, "House",  2002, # John Moakley — died in 2001
  14052, "House",  2002, # Bud Shuster — resigned in 2001
  14500, "Senate", 2000, # John Chafee — died in 1999
  15060, "House",  2002, # Norman Sisisky — died in 2001
  39508, "House",  2002, # Joe Scarborough — resigned in 2001

  # Entered Congress later through a special election or appointment
  20326, "House",  2003, # Ben Chandler — entered in 2004 special election
  20340, "House",  2003, # G.K. Butterfield — entered in 2004 special election
  20349, "House",  2003, # Stephanie Herseth Sandlin — entered in 2004 special election
  20541, "House",  2005, # Shelley Sekula-Gibbs — entered in 2006 special election
  20542, "House",  2005, # Albio Sires — entered in 2006 special election
  20749, "House",  2007, # Bill Foster — entered in 2008 special election
  20757, "House",  2007, # André Carson — entered in 2008 special election
  20759, "House",  2007, # Steve Scalise — entered in 2008 special election
  20760, "House",  2007, # Don Cazayoux — entered in 2008 special election
  20761, "House",  2007, # Travis Childers — entered in 2008 special election
  20762, "House",  2007, # Jackie Speier — entered in 2008 special election
  20763, "House",  2007, # Donna Edwards — entered in 2008 special election
  20941, "House",  2007, # Marcia Fudge — entered in 2008 special election
  20959, "House",  2009, # Ted Deutch — entered in 2010 special election
  20960, "House",  2009, # Mark Critz — entered in 2010 special election
  20961, "House",  2009, # Charles Djou — entered in 2010 special election
  20962, "House",  2009, # Tom Graves — entered in 2010 special election
  21100, "House",  2009, # Marlin Stutzman — entered in 2010 special election
  21101, "House",  2009, # Tom Reed — entered in 2010 special election
  21198, "House",  2011, # Suzanne Bonamici — entered in 2012 special election
  21199, "House",  2011, # Ron Barber — entered in 2012 special election
  21401, "House",  2013, # David Jolly — entered in 2014 special election
  21536, "House",  2013, # Donald Norcross — entered in 2014 special election
  21545, "House",  2013, # Alma Adams — entered in 2014 special election
  21553, "House",  2013, # David Brat — entered in 2014 special election
  21564, "House",  2015, # Warren Davidson — entered in 2016 special election
  21565, "House",  2015, # James Comer — entered in 2016 election to fill a vacancy
  21566, "House",  2015, # Dwight Evans — entered in 2016 special election
  21756, "House",  2017, # Conor Lamb — entered in 2018 special election
  21757, "House",  2017, # Debbie Lesko — entered in 2018 special election
  21758, "House",  2017, # Michael Cloud — entered in 2018 special election
  21759, "House",  2017, # Troy Balderson — entered in 2018 special election
  21760, "House",  2017, # Kevin Hern — entered in 2018 election to fill a vacancy
  21761, "House",  2017, # Joseph Morelle — entered in 2018 election to fill a vacancy
  21762, "House",  2017, # Mary Gay Scanlon — entered in 2018 special election
  21763, "House",  2017, # Susan Wild — entered in 2018 special election
  21764, "House",  2017, # Brenda Jones — entered in 2018 special election
  29508, "House",  2005, # Brian Bilbray — returned in 2006 special election
  31100, "House",  2011, # David Curson — entered in 2012 special election
  31101, "House",  2011, # Suzan DelBene — entered in 2012 special election
  31102, "House",  2011, # Thomas Massie — entered in 2012 special election
  31103, "House",  2011, # Donald Payne Jr. — entered in 2012 special election
  40913, "Senate", 2009, # Scott Brown — entered in 2010 special election
  40914, "Senate", 2009, # Carte Goodwin — appointed in 2010
  40915, "Senate", 2009, # Joe Manchin — entered in 2010 special election
  40916, "Senate", 2009, # Chris Coons — entered in 2010 special election
  41309, "Senate", 2013, # John Walsh — appointed in 2014
  41705, "Senate", 2017, # Doug Jones — entered in 2018 after special election
  41706, "Senate", 2017, # Tina Smith — appointed in 2018
  41707, "Senate", 2017, # Cindy Hyde-Smith — appointed in 2018
  15433, "House",  2019, # Kweisi Mfume — returned in 2020 special election
  21988, "House",  2019, # Mike Garcia — entered in 2020 special election
  21989, "House",  2019, # Tom Tiffany — entered in 2020 special election
  21990, "House",  2019, # Chris Jacobs — entered in 2020 special election
  21991, "House",  2019, # Kwanza Hall — entered in 2020 special election
  22163, "House",  2021, # Sheila Cherfilus-McCormick — entered in 2022 special election
  22164, "House",  2021, # Connie Conway — entered in 2022 special election
  22165, "House",  2021, # Mayra Flores — entered in 2022 special election
  22166, "House",  2021, # Mike Flood — entered in 2022 special election
  22167, "House",  2021, # Brad Finstad — entered in 2022 special election
  22168, "House",  2021, # Mary Peltola — entered in 2022 special election
  22169, "House",  2021, # Pat Ryan — entered in 2022 special election
  22170, "House",  2021, # Joe Sempolinski — entered in 2022 special election
  22171, "House",  2021, # Rudy Yakym — entered in 2022 election to fill a vacancy
  22377, "House",  2023, # Timothy Kennedy — entered in 2024 special election
  22378, "House",  2023, # Vince Fong — entered in 2024 special election
  22379, "House",  2023, # Michael Rulli — entered in 2024 special election
  22380, "House",  2023, # Greg Lopez — entered in 2024 special election
  22381, "House",  2023, # LaMonica McIver — entered in 2024 special election
  22382, "House",  2023, # Erica Lee Carter — entered in 2024 special election
  22383, "House",  2023, # Tony Wied — entered in 2024 election to fill a vacancy
  41904, "Senate", 2019, # Kelly Loeffler — appointed in 2020
  41905, "Senate", 2019, # Mark Kelly — entered in 2020 after special election
  42306, "Senate", 2023, # George Helmy — appointed in 2024
  20131, "House",  2001, # John Sullivan — entered in 2002 special election
  40106, "Senate", 2001  # Dean Barkley — appointed in 2002
)

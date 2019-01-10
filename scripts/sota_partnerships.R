
# 1. Pull active arts partners for each school year
partner.active <- dbGetQuery(myconn, "SELECT DISTINCT o.Id, o.Name, xref.SchoolYears
                                      FROM Organization o
                                      INNER JOIN SchoolOrganizationXref xref ON o.Id = xref.OrganizationId
                                      INNER JOIN School s ON s.Id = xref.SchoolId
                                      WHERE o.IsVisible > 0 AND o.ExcludeFromRubric = 0")

# 2. Pull active arts partners and discipline types for each school year
# This code chunk pulls disicplines for acitve partners and exlcudes those partners that don't have a disicpline listed in artlook
partner.disc <- dbGetQuery(myconn, "SELECT a.Id, a.Name, d.DisciplineType, a.SchoolYears
                                    FROM (
                                          SELECT DISTINCT o.Id, o.Name, xref.SchoolYears
                                          FROM Organization o
                                          INNER JOIN SchoolOrganizationXref xref ON o.Id = xref.OrganizationId
                                          INNER JOIN School s ON s.Id = xref.SchoolId
                                          WHERE o.IsVisible > 0 AND o.ExcludeFromRubric = 0
                                        ) a
                                     INNER JOIN OrganizationDiscipline d ON a.Id = d.OrganizationId AND a.SchoolYears = d.SchoolYears")

# 3. Pull active school-partner combos with schools, networks, and program types for each school year
# First, pull all the active school-org combinations for each school year (notice how this is different from the first query above)
partner.activecombos <- dbGetQuery(myconn, "SELECT o.Id AS OrgId, o.Name AS OrgName, s.Id AS SchoolId, s.UnitName, s.CpsNewNetwork, xref.SchoolYears
                                            FROM Organization o
                                            INNER JOIN SchoolOrganizationXref xref ON o.Id = xref.OrganizationId
                                            INNER JOIN School s ON s.Id = xref.SchoolId
                                            WHERE o.IsVisible > 0 AND o.ExcludeFromRubric = 0")
# Next, pull everything from SchoolOrganizationXrefContent
xref_content <- dbGetQuery(myconn, "SELECT * FROM SchoolOrganizationXrefContent")
# Combine the two data frames above
partner.programs <- inner_join(partner.activecombos, xref_content,
                               by = c("OrgId" = "OrganizationId", "SchoolId" = "SchoolId", "SchoolYears" = "SchoolYears"))
# Move away from sqldf and use dplyr instead
#partner.programs <- sqldf("SELECT a.OrgId, a.OrgName, a.SchoolId, a.UnitName, a.CpsNewNetwork, a.SchoolYears, c.ContentType
#                           FROM partner.activecombos a
#                           INNER JOIN xref_content c
#                           ON a.OrgId = c.OrganizationId AND a.SchoolId = c.SchoolId AND a.SchoolYears = c.SchoolYears")
partner.programs <- partner.programs[!partner.programs$SchoolId < 100000,]  # Remove "test schools"

# 4. Pull distinct school-partner combos 
# The code chunk above is helpful for getting information on program types, but it only gets us part of the way toward 
#   getting the number of active partners in each network for each school year
# Becuase the 'programs' data frame provides program type it repeats OrgName for each instance of a school-org partnership
# Now, we want to select the distinct partner, school, network, schoolyear combos
partner.distinctparts <- distinct(partner.programs, OrgId, OrgName, SchoolId, UnitName, CpsNewNetwork, SchoolYears)
parnter.distinctparts = partner.distinctparts # preserve an old typo in case combatibility is neeeded
# Move away from sqldf and use dplyr instead
#partner.distinctparts <- sqldf("SELECT DISTINCT OrgId, OrgName, SchoolId, UnitName, CpsNewNetwork, SchoolYears
#                                FROM partner.programs")

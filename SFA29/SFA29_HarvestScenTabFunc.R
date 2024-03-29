
#This chunk is to run/load the function (harvest.scen.tab()) to produce the Harvest Scenario Tables. The tables are NOT being produced in this chunk. When the function is called in the chunks below, the table produced will be for the area defined in the argument "area". The outputs in the table (e.g. Catch range, decision table data and table captions) are defined by area. Catch range (i.e. ranges of catch values displayed in the table) can be changed if needed.

#Note that table for SFA29A is formatted differently.

#Arguments:
# area - SFA29X
# catch range - define range of values shown within the table - varies for each area

#Function to produce harvest scenario tables for each area (uses packages huxtable and flextable)
sfa29.harvest.scen.tab = function(area = area, catch.range = catch.range, type = "document")
  
{

  require(huxtable) || stop("Install huxtable")
  require(flextable) || stop("Install flextable")
  require(dplyr) || stop("Install dplyr")
  
  if(area == "SFA29A") {
    catch.range <- SFA29A.decision.table$Catch.All
    decision.table <- SFA29A.decision.table
    table.caption <- paste0("Table 1. Harvest scenario table for SFA 29 West Subarea A to evaluate ",assessment.year," total subarea catch levels in terms of exploitation (\U1D452), expected changes in biomass (%), and probability (Pr.) of biomass increase. Note, Subarea A has no High suitability habitat.")
  }
  
  if(area == "SFA29B") {
    catch.range <- SFA29B.decision.table$Catch.All
    decision.table <- SFA29B.decision.table
    table.caption <- paste0("Table 2. Harvest scenario table for SFA 29 West Subarea B to evaluate ",assessment.year," total subarea catch levels in terms of exploitation (\U1D452), expected changes in biomass (%), probability (Pr.) of biomass increase, and probability of being above the Lower Reference Point (LRP: 1.12 t/km^2^) and Upper Stock Reference (USR: 2.24 t/km^2^). ")
  }
  
  if(area == "SFA29C") {
    catch.range <- SFA29C.decision.table$Catch.All
    decision.table <- SFA29C.decision.table
    table.caption <- paste0("Table 3. Harvest scenario table for SFA 29 West Subarea C to evaluate ",assessment.year," total subarea catch levels in terms of exploitation (\U1D452), expected changes in biomass (%), probability (Pr.) of biomass increase, and probability of being above the lower reference point (LRP: 1.41 t/km^2^) and upper stock reference (USR: 2.82 t/km^2^).  ")
  }
  
  if(area == "SFA29D") {
    catch.range <- SFA29D.decision.table$Catch.All
    decision.table <- SFA29D.decision.table
    table.caption <- paste0("Table 4. Harvest scenario table for SFA 29 West Subarea D to evaluate ",assessment.year," total subarea catch levels in terms of exploitation (\U1D452), expected changes in biomass (%), probability (Pr.) of biomass increase, and probability of being above the lower reference point (LRP: 1.3 t/km^2^) and upper stock reference (USR: 2.6 t/km^2^). ")
  }
  
  #FOR SFA29B, C, D create table this way:
  if(area %in% c("SFA29B", "SFA29C", "SFA29D")) {
    
    #Set range of catch for Harvest scenario table
    ex.table <<- decision.table
    if(area %in% c("SFA29B", "SFA29C")) {
      ex.table <<- dplyr::filter(ex.table, Catch.All %in% catch.range)
    }else{
      ex.table <<- dplyr::filter(ex.table, Exploit.High < 0.25) #filter(ex.table,row_number() %% 2 == 1) #filters every second row
    }
    ex.table <<- ex.table %>% mutate(across(where(is.numeric), ~ round(., 2))) %>%  #All columns 2 decimal places (Catch should be whole number)
      dplyr::select(Catch.All, Exploit.High, B.change.High, Prob.B.High, Prob_above_LRP, Prob_above_USR, B.change.All, Prob.B.All) #Re-order columns
    
    #rownames(ex.table) <- NULL
    names(ex.table) <<- c("Catch (t)", "\U1D452", "Expected\n % Change", "Pr.\n Increase", "Pr. > LRP", "Pr. > USR", "Expected\n % Change ", "Pr.\n Increase ") # the \r was added to create a unique column name - flextable doesn't work with duplicate column headers (Expected % Change and Pr. Increase).
    
    #exploitation table - as huxtable
    ex.hux <<- ex.table %>% 
      as_hux() %>% 
      theme_basic() %>% 
      set_tb_padding(0)
    
    #huxtable format - function(row#, column#):
    ex.hux <<- ex.hux %>%
      insert_row("Catch (t)", "High Habitat Suitability Category","", "","","","Whole Subarea", "", after = 0) %>% #add row with headers
      merge_cells(1, 2:6) %>%  #Merge the appropriate (rows, column:column)
      merge_cells(1,7:8) %>% #Merge the appropriate (rows, column:column)
      merge_cells(1:2,1) %>% #Merge row 1 and 2 and column 1 (Catch column header)
      set_bold(1:2, 1:8) %>% #set first two rows as bold.
      set_top_border(2, everywhere) %>% #eg. top border across rows 1 and 2 and all (everywhere) columns.
      set_bottom_border(final(1), everywhere) %>% #bottom border on last row (final), all columns
      set_top_border(1, everywhere) %>% 
      set_right_border(everywhere, c(1,6,8)) %>%
      set_all_borders(1,final(1)) %>% #Whole subarea right border not working so this fixes that.
      set_left_border(everywhere, 1) %>%
      set_number_format(everywhere, c(2,4:6,8), 2) %>%
      set_number_format(everywhere, c(3,7), 1) %>%
      set_number_format(3, 2, 0)
    
    if(type == "presentation"){
      ex.hux <<- ex.hux %>% set_font_size(16) %>% 
        set_width(2.8)
    }else{
      ex.hux <<- ex.hux %>% set_font_size(11) %>% 
        set_width(1.9)
    }
    
    ex.hux <<- ex.hux %>%
      set_align("center") %>% #Horizontal alignment of text
      set_valign("bottom") %>% #Vertical alignment of text
      set_row_height(0.1) %>%
      set_col_width(0.07) %>%
      set_all_padding(0.09) %>%
      as_flextable()
    
    if(type == "presentation"){
      ex.hux <<- ex.hux %>%
        flextable::width(j=1, width = 1) %>% #adjusting width of catch column
        flextable::width(j=2, width = 0.75)%>% 
        bg(bg = "white", part = "all")
    }else{
      ex.hux <<- ex.hux %>% 
      flextable::set_caption(table.caption, autonum = FALSE) %>%
      flextable::width(j=1, width = 1) %>% #adjusting width of catch column
      flextable::width(j=2, width = 0.75)
    }

  }
  
  if(area == "SFA29A") { #If area is SFA29A - make the table this way:
    ex.table <<- decision.table
    ex.table <<- dplyr::filter(ex.table, Catch.All %in% catch.range)
    
    ex.table <<- ex.table %>% mutate(across(where(is.numeric), ~ round(., 2))) %>%  #All columns 2 decimal places (Catch should be whole number)
      dplyr::select(Catch.All, Exploit.Medium, B.change.Medium, Prob.B.Medium, B.change.All, Prob.B.All) #Re-order columns
    
    #rownames(ex.table) <- NULL
    names(ex.table) <<- c("Catch (t)", "\U1D452", "Expected\n % Change", "Pr.\n Increase", "Expected\n % Change ", "Pr.\n Increase ") # the \r was added to create a unique column name - flextable doesn't work with duplicate column headers (Expected % Change and Pr. Increase).
    
    #exploitation table - as huxtable
    ex.hux <<- ex.table %>% 
      as_hux() %>% 
      theme_basic() %>% 
      set_tb_padding(0)
    
    #huxtable format - function(row#, column#):
    ex.hux <<- ex.hux %>%
      insert_row("Catch (t)", "Medium Habitat Suitability Category","","","Whole Subarea", "", after = 0) %>% #add row with headers
      merge_cells(1, 2:4) %>%  #Merge the appropriate (rows, column:column)
      merge_cells(1,5:6) %>% #Merge the appropriate (rows, column:column)
      merge_cells(1:2,1) %>% #Merge row 1 and 2 and column 1 (Catch column header)
      set_bold(1:2, 1:6) %>% #set first two rows as bold.
      set_top_border(2, everywhere) %>% #eg. top border across rows 1 and 2 and all (everywhere) columns.
      set_bottom_border(final(1), everywhere) %>% #bottom border on last row (final), all columns
      set_top_border(1, everywhere) %>% 
      set_right_border(everywhere, c(1,4,6)) %>%
      set_all_borders(1,final(1)) %>% #Whole subarea right border not working so this fixes that.
      set_left_border(everywhere, 1)
    
    if(type == "presentation"){
      ex.hux <<- ex.hux %>% set_font_size(16) %>% 
      set_width(4.4)
    }else{
      ex.hux <<- ex.hux %>% set_font_size(11) %>% 
        set_width(2.8)
    }
    ex.hux <<- ex.hux %>%
      set_number_format(everywhere, c(2,4:6), 2) %>%
      set_number_format(everywhere, c(3,5), 1) %>%
      set_number_format(3, 2, 0) %>%
      set_align("center") %>% #Horizontal alignment of text
      set_valign("bottom") %>% #Vertical alignment of text
      set_row_height(0.1) %>%
      set_col_width(0.07) %>%
      
      as_flextable()    
      
      if(type == "presentation"){
        ex.hux <<- ex.hux %>%
          flextable::width(j=1, width = 1) %>% #adjusting width of catch column
          flextable::width(j=2, width = 0.75) %>% 
          bg(bg = "white", part = "all")
      }else{
        ex.hux <<- ex.hux %>% 
          flextable::set_caption(table.caption, autonum = FALSE) %>%
          flextable::width(j=1, width = 1) %>% #adjusting width of catch column
          flextable::width(j=2, width = 0.75)
      }
    
  }
}

#test <- sfa29.harvest.scen.tab(area = "SFA29B", catch.range = catch.range, type = "document")
#ex.hux
#ex.table
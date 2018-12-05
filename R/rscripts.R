# rscripts.R ----------------------------------------------------------------


# Header
# Filename:       rscripts.R
# Description:    Contains functions generating various R scripts.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     23 May 2017
# Last Revision:  12 February 2017
# Version:        0.2.3
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     23 May 2017         Initial issue for TFD3 syncing observers
# 0.1.0     25 July 2017        Function TFD3.observer.table.S2C.R() modified: supports triggering a replot if sync and report tables have different structures
# 0.1.1     25 July 2017        TFD3.service() modified: re-plots if sync$itemID_trigger changes
# 0.1.2     29 August 2017      TFD3.observer.table.S2C.R() modified: Small bug rectified: Changing cell value to/from NA did not trigger a change and setCellValue() was not called.
# 0.1.3     12 October 2017     Observer function TFD3.observer.column.footer.R() changed. Code protected by isolate to be called only when sync$<itemID>_column.footer changes
# 0.1.5     23 October 2017     Observer function TFD3.observer.table.S2C.R() modified: Table redraw is trigerred if any cell is replaced to/from NA
# 0.1.6     26 October 2017     Observer function TFD3.observer.table.S2C.R() modified: Bug fixed! Column number was shifted for tables with rownames
# 0.1.7     27 October 2017     Observer function TFD3.observer.table.S2C.R() modified: Calls function compareTable() from niragen to extract changes to be sent
# 0.1.8     27 November 2017    loginService and loginUIExtraService for loginInput added
# 0.1.9     06 December 2017    'login.srv' and 'logout.srv' modified: Saves/loads session data after logout/login"
# 0.2.0     08 December 2017    'login.srv' and 'logout.srv' modified: saves/loads local objects in userData
# 0.2.1     08 December 2017    'login.srv' and 'logout.srv' modified: user is saved in session$user as well as sync$user
# 0.2.2     15 December 2017    'login.srv' modified: Sends a different fail message for when username is not in the 'loginTable'. usernames are rownames of property 'loginTable'
# 0.2.3     12 February 2018    'login.srv' and 'logout.srv' modified: session data is saved in the path given by settings$savePath rather than the working directory (adding more flexibility to the package user)


# Only server to client!
TFD3.observer.column.footer.R = function(itemID){paste0("
                                                        if(is.null(sync$", itemID, "_column.footer)){sync$", itemID, "_column.footer <- items[['", itemID, "']]$config$column.footer}
                                                        isolate({
                                                        nms = c('rownames', colnames(sync$", itemID, "))
                                                        for (col in names(sync$", itemID, "_column.footer)){
                                                        wch = which(nms == col) - 1
                                                        if   (inherits(sync$", itemID, "_column.footer[[col]], 'function')){val = sapply(list(sync$", itemID, "[report$", itemID, "_filtered, col]), sync$", itemID, "_column.footer[[col]])}
                                                        else {val = sync$", itemID, "_column.footer[[col]] %>% as.character}
                                                        for (cn in wch){if(!is.empty(val)){setFootCellValue(session, tbl = '", itemID, "', row = 1, col = cn, value = val)}}
                                                        }
                                                        })
                                                        ")}

# server to client
TFD3.observer.column.editable.R = function(itemID){paste0("

                                                          if(is.null(sync$", itemID, "_column.editable)){sync$", itemID, "_column.editable <- items[['", itemID, "']]$config$column.editable}
                                                          isolate({
                                                          wrnflag = items[['", itemID, "']]$config$withRowNames
                                                          if(wrnflag %>% is.empty){wrnflag = T}
                                                          enacols = sync$", itemID, "_column.editable %>% unlist %>% coerce('logical') %>% which %>% names %>% intersect(c('rownames', colnames(sync$", itemID, ")))
                                                          discols = c('rownames', colnames(sync$", itemID, ")) %-% enacols
                                                          for(col in enacols){
                                                          if (col == 'rownames'){
                                                          if(wrnflag){enableEdit(session, '", itemID, "', 'col_0')}
                                                          } else {
                                                          w = which(names(sync$", itemID, ") == col) - !wrnflag
                                                          enableEdit(session, '", itemID, "', 'col_' %++% w);
                                                          }
                                                          }

                                                          for(col in discols){
                                                          if (col == 'rownames'){
                                                          if(wrnflag){disableEdit(session, '", itemID, "', 'col_0')}
                                                          } else {
                                                          w = which(names(sync$", itemID, ") == col) - !wrnflag
                                                          disableEdit(session, '", itemID, "', 'col_' %++% w);
                                                          }
                                                          }
                                                          #debug(check)
                                                          #check(x = 'editable', y = sync$", itemID, "_column.editable, z = enacols, t = discols, r = items[['", itemID, "']])
                                                          })
                                                          ")
}

# client to server:
TFD3.observer.edit.R = function(itemID) {paste0("
                                                if(is.null(input$", itemID, "_edit)) return(NULL);
                                                edit <- input$", itemID, "_edit;
                                                isolate({
                                                # need isolate, otherwise this observer would run twice
                                                # for each edit
                                                id  <- edit$id;
                                                row <- as.integer(edit$row);
                                                col <- as.integer(edit$col);
                                                val <- edit$val;
                                                nms <- colnames(sync$", itemID, ")

                                                if(col == 0) {
                                                oldval <- rownames(sync$", itemID, ")[row];
                                                cellClass = 'character'
                                                fltr = items[['", itemID, "']]$config$column.acceptor[['rownames']]}
                                                else {
                                                oldval <- sync$", itemID, "[row, col];
                                                fltr = items[['", itemID, "']]$config$column.acceptor[[nms[col]]]
                                                cellClass = class(sync$", itemID, "[, col])[1]
                                                }
                                                val0   = val
                                                if (cellClass == 'POSIXct') {val = try(as.POSIXct(val, format = '%Y-%m-%dT%H:%M:%S', tz = 'GMT'), silent = T)} else {val = try(coerce(val, cellClass), silent = T)}

                                                accept = inherits(val, cellClass) & !is.empty(val)

                                                if(accept & !is.empty(fltr)){
                                                if(      inherits(fltr, 'character')){txt = paste('val', fltr, collapse = ' & ')}
                                                else if (inherits(fltr, 'list'))     {txt = rScriptFilter('val', fltr)}
                                                else if (inherits(fltr, 'function')) {txt = 'val %>% sapply(fltr)'}
                                                else                                 {txt = 'TRUE'}
                                                accept = parse(text = txt) %>% eval
                                                if(!inherits(accept, 'logical')){accept = T}
                                                }

                                                if (accept){
                                                if(col == 0) {
                                                rownames(sync$", itemID, ")[row] <- val;
                                                rownames(report$", itemID, ")[row] <- val;
                                                } else {
                                                shp = items[['", itemID, "']]$config$column.shape[[nms[col]]]
                                                if (!is.null(shp)){
                                                if(shp == 'radioButtons'){
                                                sync$", itemID, "[, col] <- FALSE;
                                                report$", itemID, "[, col] <- FALSE;
                                                }
                                                }
                                                # debug(check); check(x = 'Angoolak kardi!', y = input$", itemID, "_edit, z = val, r = row, t = col, s = sync$", itemID, ")
                                                sync$", itemID, "[row, col] <- val;
                                                report$", itemID, "[row, col] <- val;

                                                }
                                                # confirm edits
                                                confirmEdit(session, tbl = '", itemID, "', row = row, col = col, id = id, value = val);
                                                report$", itemID, "_lastEdits['Success', 'Row'] <- row;
                                                report$", itemID, "_lastEdits['Success', 'Column'] <- col;
                                                report$", itemID, "_lastEdits['Success', 'Value'] <- val;
                                                } else {

                                                # debug(check)
                                                # check(x = accept, y = fltr, z = val, t = val0, r = oldval, s = list(row, col, id))

                                                rejectEdit(session, tbl = '", itemID, "', row = row, col = col,  id = id, value = oldval);
                                                report$", itemID, "_lastEdits['Fail', 'Row'] <- row;
                                                report$", itemID, "_lastEdits['Fail', 'Column'] <- col;
                                                report$", itemID, "_lastEdits['Fail', 'Value'] <- val0;
                                                }
                                                })
                                                ")}

# Use it later for creating the default footer:
# footer = list('Mean', object[[i]] %>% colMeans %>% as.matrix %>% t) %>% as.data.frame
# names(footer) = c('Rownames', colnames(object[[i]]))

# Client 2 Server: FOB1
TFD3.observer.filter.C2S.R = function(itemID){
  paste0("
         if(is.null(input$", itemID, "_filter)){return(NULL)}
         isolate({
         report$", itemID, "_filtered <- unlist(input$", itemID, "_filter$validRows);
         sync$", itemID, "_column.filter = list()
         nms = c('rownames', colnames(sync$", itemID, "))
         # lapply(input$", itemID, "_filter$filterSettings, function(x) )
         for(flt in input$", itemID, "_filter$filterSettings){
         colnumb = flt$column %>% substr(5, nchar(flt$column)) %>% as.integer
         colname = nms[colnumb]
         if(!is.na(colname)){sync$", itemID, "_column.filter[[colname]] = chif(is.empty(flt$value), NULL, flt$value)}
         # debug(check)
         # check('FOB1', colnumb, colname, input$", itemID, "_filter$filterSettings, flt, sync$", itemID, "_column.filter)
         }
         # report$", itemID, "_column.filter = sync$", itemID, "_column.filter
         })
         ")
}

#  Server 2 Client: FOB2
TFD3.observer.filter.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, "_column.filter)){sync$", itemID, "_column.filter = items[['", itemID, "']]$config$column.filter}
         isolate({
         for(flt in input$", itemID, "_filter$filterSettings){
         nms = c('rownames', colnames(sync$", itemID, "))

         colnumb = flt$column %>% substr(5, nchar(flt$column)) %>% as.integer
         colname = nms[colnumb]
         colnumb = colnumb - 1

         if (colname %in% names(sync$", itemID, "_column.filter)){
         if (!identical(flt$value, sync$", itemID, "_column.filter[[colname]])){
         # set filter
         setFilter(session, tbl = '", itemID, "', col = 'col_' %++% colnumb, filterString = sync$", itemID, "_column.filter[[colname]], doFilter = TRUE);
         }
         # else {do nothing}
         } else {
         setFilter(session, tbl = '", itemID, "', col = 'col_' %++% colnumb, filterString = '', doFilter = TRUE);
         }
         # debug(check)
         # check('FOB2', y = input$", itemID, "_filter$filterSettings, z = colnumb, t = colname, r = flt, s = sync$", itemID, "_column.filter)
         # report$", itemID, "_column.filter = sync$", itemID, "_column.filter
         }
         })
         ")
}

# client to server: sob1
TFD3.observer.selected.C2S.R = function(itemID){
  paste0("
         if(is.null(input$", itemID, "_select)){return(NULL)}
         isolate({
         sync$", itemID, "_selected = input$", itemID, "_select
         report$", itemID, "_selected = sync$", itemID, "_selected
         })
         ")
}


# server 2 client: sob2
TFD3.observer.selected.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, "_selected)){sync$", itemID, "_selected = items[['", itemID, "']]$config$selected}
         isolate({
         if(is.null(report$", itemID, "_selected)){report$", itemID, "_selected = items[['", itemID, "']]$config$selected}
         if(is.null(sync$", itemID, "_row.color)){sync$", itemID, "_row.color = items[['", itemID, "']]$config$row.color}
         sel   = sync$", itemID, "_selected %-% report$", itemID, "_selected
         desel = report$", itemID, "_selected %-% sync$", itemID, "_selected
         for (i in sel){  setRowClass(session, tbl = '", itemID, "', row = i, class = items['", itemID, "']$config$selection.color)}
         for (i in desel){setRowClass(session, tbl = '", itemID, "', row = i, class = chif(sync$", itemID, "_row.color[i] == items['", itemID, "']$config$selection.color, '', items[['", itemID, "']]$config$row.color[i]))}
         report$", itemID, "_selected = sync$", itemID, "_selected
         })
         ")
}


# server 2 client: for row color: cob2
TFD3.observer.color.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, "_row.color)){sync$", itemID, "_row.color = items[['", itemID, "']]$config$row.color}
         isolate({
         # debug(check)
         # check(x = 'cob2', y = sync$", itemID, "_row.color, z = report$", itemID, "_row.color, t = sync$", itemID, ")
         if(is.null(report$", itemID, "_row.color)){report$", itemID, "_row.color = items[['", itemID, "']]$config$row.color}
         w = which(sync$", itemID, "_row.color != report$", itemID, "_row.color)
         for (i in w){setRowClass(session, tbl = '", itemID, "', row = i, class = sync$", itemID, "_row.color[i])}
         report$", itemID, "_row.color = sync$", itemID, "_row.color
         })
         ")
}


# server to client: for table contents: tob2
TFD3.observer.table.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, ")){
         sync$", itemID, " = items[['", itemID, "']]$data
         }
         isolate({
         if(is.null(report$", itemID, ")){report$", itemID, " <- items[['", itemID, "']]$data}
         if(!identical(report$", itemID,", sync$", itemID, ")){
         cmp = compareTables(report$", itemID,", sync$", itemID, ")
         # if(length(cmp)>0){debug(check); check(x = 'I am editing', y = cmp, z = items[['", itemID, "']]$config, r = report$", itemID,", s = sync$", itemID, ", t = session)}
         if(!is.null(cmp)){
         for(df in cmp){
         for(i in sequence(nrow(df))){
         setCellValue(session, tbl = '", itemID, "', row = df$row[i], col = df$col[i], value = df$to[i], feedback = FALSE)
         }
         }
         }
         # debug(check); check(x = 'I am changing items data', y = cmp, z = items, r = report, s = sync, t = session)
         items[['", itemID, "']]$data <<- sync$", itemID, "
         report$", itemID, "          <- sync$", itemID, "

         if(is.null(cmp)){sync$", itemID, "_trigger = T}

         }
         })
         ")
}

# server to client: for table contents: tob2
TFD3.observer.table.S2C.R.old = function(itemID){
  paste0("
         if(is.null(sync$", itemID, ")){sync$", itemID, " = items[['", itemID, "']]$data}
         isolate({
         if(is.null(report$", itemID, ")){report$", itemID, " <- items[['", itemID, "']]$data}

         trig = ncol(sync$", itemID, ") != ncol(report$", itemID,")
         trig = trig | nrow(sync$", itemID, ") != nrow(report$", itemID,")
         # trig = trig | rownames(sync$", itemID, ") != rownames(report$", itemID,")
         # trig = trig | colnames(sync$", itemID, ") != colnames(report$", itemID,")
         if(!trig){trig = length(which(is.na(sync$", itemID, ") != is.na(report$", itemID, "))) > 0}

         if (trig){
         items[['", itemID, "']]$data <<- sync$", itemID, "
         report$", itemID, " <- sync$", itemID, "
         } else {

         for (i in sequence(ncol(sync$", itemID, "))){
         # Check for classes of columns 2b identical, otherwise trigger a re-plot
         # todo: If you the column type is factor with different levels, an error is caused here! Either use character columns or convert them to character here before comparing
         ss = sync$", itemID, "[,i]
         rr = report$", itemID, "[,i]
         trig = !inherits(ss, rr %>% class)
         if(is.factor(ss)){trig = trig | !(levels(ss) %==% levels(rr))}

         if(!trig){
         w = which(ss != rr)
         # w = c(w, which((is.na(ss) & !is.na(rr))|(is.na(rr) & !is.na(ss))))
         # w = w - chif(items[['", itemID, "']]$config$withRowNames,0,1)
         for (j in w) {
         setCellValue(session, tbl = '", itemID, "', row = j, col = i, value = sync$", itemID, "[j,i], feedback = TRUE)
         report$", itemID, "[j,i] <- sync$", itemID, "[j,i]
         }
         }
         }
         rnew = rownames(sync$", itemID, ")
         rold = rownames(report$", itemID, ")
         w    = which(rnew != rold)

         for (j in w) {
         setCellValue(session, tbl = '", itemID, "', row = j, col = 0, value = rnew[j], feedback = TRUE)
         }
         rownames(report$", itemID, ") <- rnew
         }
         if(trig){sync$", itemID, "_trigger = T}
         })
         ")
}

TFD3.service = function(itemID){
  paste0("
         if(sync[['", itemID, "_trigger']]){
         sync[['", itemID, "_trigger']] = F
         }
         isolate({
         sync[['", itemID, "']] %>% TFD3.table(config = items[['", itemID, "']]$config)})
         ")
}


loginBox = "div(
textInput('getUser', 'Username'),
passwordInput('getPass', 'Password'),
br(),
actionButton('login', 'Log in')
  )"

logoutBox =
  "div(
' Logged in as:', strong(session$user),
actionLink('logout', label = 'Log out', icon = shiny::icon('sign-out'))
  )
"
loginUIService = paste0("if(sync$user %>% is.null){", loginBox, "} else {", logoutBox, "}")

login.srv = "if(!is.empty(input$getUser) & !is.empty(input$getPass)){
res = loginTable %>% loginVerify(input$getUser, input$getPass, settings$passEncryption)
if(inherits(res, 'character')){sync$message = messages['loginSuccess']; sync$user = input$getUser; session$user = input$getUser; if(settings$saveSession){flnm = settings$savePath %++% session$user %++% '.rds'; if(file.exists(flnm)){sessiondata = readRDS(flnm);for(i in names(sessiondata$local)){session$userData[[i]] = sessiondata$local[[i]]}; for (i in names(sessiondata$sync)){sync[[i]] <- sessiondata$sync[[i]]}}}} else {session$user = NULL; if(res == 1){sync$message = messages['loginFail']} else if(res == 2){sync$message = messages['usernameNotFound']} else {sync$message = 'This should not happen!!'} }
}"

logout.srv = "if(settings$saveSession){lc = list(); for (i in names(session$userData)){lc[[i]] <- session$userData[[i]]}; ss = list(); for (i in names(sync)){ss[[i]] <- sync[[i]]}; sessiondata = list(local = lc, sync = ss); saveRDS(sessiondata, settings$savePath %++% session$user %++% '.rds')}; session$user = NULL; sync$user = NULL"

loginUIExtraService = paste("observeEvent(input$login, {", login.srv, "})", "observeEvent(input$logout, {", logout.srv, "})", sep = '\n')



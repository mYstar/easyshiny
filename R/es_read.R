library(dplyr)
library(tidyr)

### --------------------------------------- ###
### generic functions
### --------------------------------------- ###
# reads a fileset from folder (shiny fileinput format)
base.data.read.filesets <- function( folders ) {

  data.frame( datapath = dir(folders, full.names = TRUE) ) %>%
    tbl_df() %>%
    mutate(datapath = datapath %>% as.character) %>%
    mutate( folder = dirname(datapath) ) %>%
    mutate( name = basename(datapath) ) %>%
    mutate( n = group_indices(., folder) )
}

base.data.read.files <- function( filesets, filename, callback,... ) {
  filesets %>%
    group_by(n) %>%
    do( {
        filedata <- filter(., name == paste0(filename, '.rds') )
        if( filedata %>% nrow() != 0)
          result <- readRDS(filedata$datapath)
        else {
          filedata <- filter(., name == paste0(filename, '.csv') )
          result <- filedata$datapath %>%
            read.csv(..., as.is = TRUE) %>%
            tbl_df %>%
            callback()
        }
        result
    } ) %>%
    ungroup
}

base.data.read.exfiles <- function( filesets, filename, callback ) {
  base.data.read.files( filesets, filename, callback, header = FALSE )
}

base.data.add.setname <- function( simdata, setnames ) {

  # create numbered DF from setnames
  setnames <- data.frame(setname = setnames) %>%
    mutate( n = row_number() ) %>%
    tbl_df()

  simdata %>%
    left_join( setnames )
}

# select the data inside the timerange
base.data.select <- function( simdata , time_min, time_max ) {
  simdata %>%
    filter(Simulation.Time >= time_min & Simulation.Time <= time_max)
}
### --------------------------------------- ###
### Machine Data
### --------------------------------------- ###
# machine state
base.data.read.machines.states <- function( filesets ) {
  base.data.read.files( filesets, file.machines_states, callback = function(raw_data) {
    raw_data %>%
      gather( 'machine', 'Percentage', starts_with('ANLAGE') ) %>%
      separate( machine, c('machine', 'state'), sep = '\\.Statistics\\.' ) %>%
      mutate( machine = sub('ANLAGE\\.', '', machine) ) %>%
      mutate( state = sub('Percentage', '', state) ) %>%
      mutate( state = sub('State', '', state) ) %>%
      mutate( machine =  machine %>%
                as.integer() %>%
                formatC(width = 2, format = 'd', flag = '0')
            )
  } )
}

# machine produced tubes
base.data.read.machines.tubes <- function( filesets ) {
  base.data.read.machines.boxes( filesets ) %>%
      filter((Simulation.Time+60)%%86400 == 0) %>%
      group_by(n, machine) %>%
      mutate( tubes_per_day = TubesProduced - lag(TubesProduced, default = 0) ) %>%
      ungroup()
}

# boxes at machines
base.data.read.machines.boxes <- function( filesets ) {
  base.data.read.files(filesets, file.machines_boxes, callback = function(raw_data) {
    raw_data %>%
      gather("variable", "value", starts_with('ANLAGE') ) %>%
      mutate( machine = sub('ANLAGE\\.(.*)\\..*', '\\1', variable) ) %>%
      mutate( variable = sub('ANLAGE\\..*\\.', '', variable)) %>%
      spread(variable, value) %>%
      mutate( machine =  machine %>%
                as.integer() %>%
                formatC(width = 2, format = 'd', flag = '0')
            )
  } )
}

## length of simulation
base.data.time.max <- function( simdata ) {
  simdata %>%
    filter( Simulation.Time == max(Simulation.Time) ) %>%
    select( Simulation.Time ) %$%
    .[1,1] %>%
    as.integer
}
## calculate warmup
base.data.time.warmup <- function( machine_data ) {
  machine_data %>%
    filter(CurrentBoxesStored > 0) %>%
    group_by(machine) %>%
    filter( Simulation.Time == min(Simulation.Time) ) %>%
    ungroup %>%
    filter( Simulation.Time == max(Simulation.Time) ) %>%
    select( Simulation.Time ) %$%
    .[1,1] %>%
    as.integer
}

### --------------------------------------- ###
### QC data
### --------------------------------------- ###
base.data.read.qc.cycles <- function( filesets ) {
  base.data.read.files( filesets, file.qc_cycles, callback = function(raw_data) {raw_data} )
}
base.data.read.qc.buffer <- function( filesets ) {
  base.data.read.files( filesets, file.qc_buffer, callback = function(raw_data) {raw_data} )
}

### --------------------------------------- ###
### box stacker
### --------------------------------------- ###
base.data.read.stacker.boxes <- function( filesets ) {
  base.data.read.exfiles( filesets, file.box_stacker, callback = function(raw_data) {
    raw_data %>%
      mutate(Simulation.Time = V1) %>%
      select(-V1) %>%
      mutate( rnum = row_number() ) %>%
      gather('col', 'product', starts_with('V')) %>%
      filter(product!="") %>%
      group_by(rnum, Simulation.Time, product) %>%
      summarise( count = n()) %>%
      ungroup()
  } )
}

### --------------------------------------- ###
### Transferlids
### --------------------------------------- ###
base.data.read.transferlids <- function( filesets ) {
  base.data.read.files( filesets, file.transferlids, callback = function(raw_data) {raw_data} )
}

### --------------------------------------- ###
### Warehouse
### --------------------------------------- ###
# pallets
base.data.read.warehouse.pallets <- function( filesets ) {
  base.data.read.files( filesets, file.warehouse_pallets, callback = function(raw_data) {
    raw_data %>%
      gather( "pallettype", "pallets", starts_with('Warehouse') ) %>%
      mutate( pallettype = substring(pallettype, 11))
  } )
}

# output to line
# helper function: counts the changes (insert) that have to be made to
# transform list1 to list2
remove_duplicates <- function( output ) {
  data <- output %>%
    select( starts_with('V'))

  for(rowidx in nrow(data):2) {
    lastrow <- data[rowidx-1,]
    currrow <- data[rowidx,]

    expected <- 1
    for(colidx in 1:ncol(data)) {
      found <- match(currrow[colidx], lastrow)
      if( !is.na(found) && found >= expected) {
        data[rowidx, colidx] <- ""
        expected <- found + 1
      }
    }
  }
  output %>%
    select(Simulation.Time) %>%
    bind_cols(data)
}

base.data.read.warehouse.montage_manual <- function( filesets ) {
  manual <- base.data.read.exfiles( filesets, file.montage_manual, callback = function(raw_data) {
    raw_data %>%
      mutate(Simulation.Time = V1) %>%
      select(-V1) %>%
      remove_duplicates() %>%
      gather('col', 'boxtype', starts_with('V')) %>%
      filter(boxtype!="") %>%
      group_by(Simulation.Time, boxtype) %>%
      summarise( count = n() ) %>%
      ungroup() %>%
      mutate(pallettype = 'boxes_manual')
  } )
}

base.data.read.warehouse.montage_auto <- function( filesets ) {
  auto <- base.data.read.exfiles( filesets, file.montage_auto, callback = function(raw_data) {
    raw_data %>%
      mutate(Simulation.Time = V1) %>%
      select(-V1) %>%
      remove_duplicates() %>%
      gather('col', 'boxtype', starts_with('V')) %>%
      filter(boxtype!="") %>%
      group_by(Simulation.Time, boxtype) %>%
      summarise( count = n() ) %>%
      ungroup() %>%
      mutate(pallettype = 'boxes_auto')
  } )
}

base.data.read.warehouse.caps <- function( filesets ) {
  caps <- base.data.read.exfiles( filesets, file.caps, callback = function(raw_data) {
    raw_data %>%
      mutate(Simulation.Time = V1) %>%
      select(-V1) %>%
      remove_duplicates() %>%
      gather('col', 'boxtype', starts_with('V')) %>%
      filter(boxtype!="") %>%
      group_by(Simulation.Time, boxtype) %>%
      summarise( count = n() ) %>%
      ungroup() %>%
      mutate(pallettype = 'caps')
  } )
}

base.data.read.warehouse.output <- function( filesets ) {
   base.data.read.warehouse.montage_manual(filesets) %>%
    bind_rows( base.data.read.warehouse.montage_auto(filesets) ) %>%
    bind_rows( base.data.read.warehouse.caps(filesets) )
}

### --------------------------------------- ###
### lifter utilization
### --------------------------------------- ###
base.data.read.lifter <- function( filesets ) {
  base.data.read.files( filesets, file.lifter, callback = function(raw_data) {
    raw_data %>%
      gather("lifternumber", "percentage", starts_with('Lifter_')) %>%
      mutate( state = sub('.*Statistics\\.', '', lifternumber)) %>%
      mutate( state = sub('Percentage', '', state)) %>%
      mutate( lifternumber = sub('Lifter_([0-9].*)\\.Statistics.*', '\\1', lifternumber) ) %>%
      filter( percentage > 0.0)
  } )
}

### --------------------------------------- ###
### loop conveyor
### --------------------------------------- ###
base.data.read.loop <- function( filesets ) {
  base.data.read.files( filesets, file.loop, callback = function(raw_data) {
    raw_data %>%
      gather("boxtype", "count", starts_with('ConveyorLineController')) %>%
      mutate( boxtype = sub('ConveyorLineController.Counter_Box_', '', boxtype) )
  } )
}

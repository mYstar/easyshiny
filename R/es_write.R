library(magrittr)
library(dplyr)
library(tidyr)

# converts a given folders files to the .rds format
convert_rds <- function(folder) {

  # create destination folder
  folder <-  sub('/$', '', folder) # remove last / if present
  new_folder <- paste0(folder, '_rds')
  dir.create(new_folder)
  folder <- paste0(folder, '/')

  files <- base.data.read.filesets(folder)

  # read the files
  machines_boxes <- base.data.read.machines.boxes( files ) %>%
    select(-n)
  machines_states <- base.data.read.machines.states( files ) %>%
    select(-n)
  qc_cycles <- base.data.read.qc.cycles( files ) %>%
    select(-n)
  qc_buffer <- base.data.read.qc.buffer( files ) %>%
    select(-n)
  stacker_boxes <- base.data.read.stacker.boxes( files ) %>%
    select(-n)
  transferlids <- base.data.read.transferlids( files ) %>%
    select(-n)
  warehouse_pallets <- base.data.read.warehouse.pallets( files ) %>%
    select(-n)
  montage_manual <- base.data.read.warehouse.montage_manual( files ) %>%
    select(-n)
  montage_auto <- base.data.read.warehouse.montage_auto( files ) %>%
    select(-n)
  caps <- base.data.read.warehouse.caps( files ) %>%
    select(-n)
  lifter_states <- base.data.read.lifter( files ) %>%
    select(-n)
  loop_boxes <- base.data.read.loop( files ) %>%
    select(-n)

  # write the rds files
  saveRDS(machines_boxes, file=paste0(new_folder, '/', file.machines_boxes, '.rds'))
  saveRDS(machines_states, file=paste0(new_folder, '/', file.machines_states, '.rds'))
  saveRDS(qc_cycles, file=paste0(new_folder, '/', file.qc_cycles, '.rds'))
  saveRDS(qc_buffer, file=paste0(new_folder, '/', file.qc_buffer, '.rds'))
  saveRDS(stacker_boxes, file=paste0(new_folder, '/', file.box_stacker, '.rds'))
  saveRDS(transferlids, file=paste0(new_folder, '/', file.transferlids, '.rds'))
  saveRDS(warehouse_pallets, file=paste0(new_folder, '/', file.warehouse_pallets, '.rds'))
  saveRDS(montage_manual, file=paste0(new_folder, '/', file.montage_manual, '.rds'))
  saveRDS(montage_auto, file=paste0(new_folder, '/', file.montage_auto, '.rds'))
  saveRDS(caps, file=paste0(new_folder, '/', file.caps, '.rds'))
  saveRDS(lifter_states, file=paste0(new_folder, '/', file.lifter, '.rds'))
  saveRDS(loop_boxes, file=paste0(new_folder, '/', file.loop, '.rds'))
}

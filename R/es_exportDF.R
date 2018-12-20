library(dplyr)

read_data_frames <- function(folders, setnames) {

  genv <- globalenv()
  genv$filesets <- base.data.read.filesets(folders)

  # machines
  genv$machines_boxes <- base.data.read.machines.boxes( filesets ) %>%
    base.data.add.setname( setnames )
  genv$machines_states <- base.data.read.machines.states( filesets ) %>%
    base.data.add.setname( setnames )
  genv$machines_tubes <- base.data.read.machines.tubes( filesets ) %>%
    base.data.add.setname( setnames )
  genv$time_max <- base.data.time.max( machines_boxes )
  genv$time_warmup <- base.data.time.warmup( machines_boxes )

  # QC
  genv$qc_cycles <- base.data.read.qc.cycles( filesets ) %>%
    base.data.add.setname( setnames )
  genv$qc_buffer <- base.data.read.qc.buffer( filesets ) %>%
    base.data.add.setname( setnames )

  # box stacker
  genv$stacker_boxes <- base.data.read.stacker.boxes( filesets ) %>%
      base.data.add.setname( setnames )

  # transferlids
  genv$transferlids <- base.data.read.transferlids( filesets ) %>%
      base.data.add.setname( setnames )

  # warehouse
  genv$warehouse_pallets <- base.data.read.warehouse.pallets( filesets ) %>%
      base.data.add.setname( setnames )
  genv$warehouse_output <- base.data.read.warehouse.output( filesets ) %>%
      base.data.add.setname( setnames )

  # lifter
  genv$lifter_states <- base.data.read.lifter(filesets) %>%
      base.data.add.setname( setnames )

  # loop
  genv$loop_boxes <- base.data.read.loop(filesets) %>%
      base.data.add.setname( setnames )
}

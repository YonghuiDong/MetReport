#' randomizeSeq
#' @param nWithinBlank the number of blanks within the samples, i.e., after QC.
#' @description A fct function
#' @importFrom dplyr %>% row_number add_row bind_rows
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @examples
#' df <- data.frame(Group = rep(c("A", "B", "C"), each = 20), Rep = rep(1:6, times = 10))
#' result <- randomizeSeq(df, nBlank = 0, nQC = 6, nWithinBlank = 0, nEmptyCell = 2)

randomizeSeq <- function(df, nBlank = 0, nQC = 0, nWithinBlank = 0, nEmptyCell = 0, plateRow = 6, plateCol = 8, plateIDType = "Letter", outputType = "Orbitrap") {
  colnames(df) <- c("Group", "Rep")
  ## Best block is designed when the replicates are not the same among different groups
  bestBlock <- df %>%
    dplyr::count(Group) %>%
    dplyr::summarize(Fre = names(which.max(table(n)))) %>%
    as.numeric()

  df2 <- df %>%
    dplyr::mutate(Group = as.factor(Group)) %>%
    dplyr::mutate(Sample = paste(Group, Rep, sep = "_")) %>%
    dplyr::mutate(bin = as.numeric(Group)) %>%
    dplyr::group_by(bin) %>%
    dplyr::mutate(Block = rep(1:bestBlock, length.out = n())) %>%
    dplyr::group_by(Block) %>%
    dplyr::sample_n(n()) %>%
    dplyr::select(-bin, -Rep) %>%
    dplyr::ungroup()

  ## add QC
  if(nQC != 0){
    new_row <- data.frame(Group = c("QC", rep("Blank", nWithinBlank)), Sample = c("QC", rep("Blank", nWithinBlank)))

    df2 <- df2 %>%
      dplyr::slice(c(1:(nQC-1), nQC:nrow(.))) %>% # Slice into two parts
      dplyr::group_by(group = (seq_len(nrow(.)) - 1) %/% nQC + 1) %>% # Group by every n rows
      dplyr::do(bind_rows(., new_row)) %>% # Add new row after every n rows
      dplyr::ungroup() %>% # Ungroup the data
      dplyr::select(-group) %>%
      dplyr::group_by(Sample) %>%
      dplyr::mutate(Sample = ifelse(Sample == "QC", paste0("QC_", (row_number()+1)), Sample),
                    Sample = ifelse(Sample == "Blank", paste0("Blank_", (row_number() + nBlank)), Sample)) %>%
      dplyr::ungroup() %>%
      dplyr::add_row(Group = "QC", Sample = "QC_1", .before = 1) # add QC1 before samples
  }

  ## add Black
  if(nBlank != 0){
    df2 <- df2 %>%
      dplyr::add_row(Group = rep("Blank", nBlank), Sample = paste0("Blank_", 1:nBlank), .before = 1)
  }

  ## add sample plate position
  if(outputType == "Orbitrap"){
    combinations <- paste0(rep(LETTERS[1:26], each = plateCol), 1:plateCol)[1: (plateRow * plateCol)]
  }
  if(outputType == "Waters"){
    combinations <- paste0(rep(LETTERS[1:26], each = plateCol), ",", 1:plateCol)[1: (plateRow * plateCol)]
  }

  ## add sequence position
  if(plateIDType == "Letter"){repeated_ID = rep(LETTERS[1:26], length.out = nrow(df2))}
  if(plateIDType == "Number"){repeated_ID = rep(1:26, length.out = nrow(df2))}
  nPlate <- ceiling(nrow(df2)/(plateRow * plateCol)) # determine no. of max plate
  finalSeq <- paste(rep(repeated_ID[1:nPlate], each = length(combinations)), combinations, sep = ":") # get total sequence position
  positionHolder <- max(
    (as.numeric(nQC > 0) + as.numeric(nBlank > 0) + nEmptyCell),
    (as.numeric(nQC > 0) + as.numeric(nWithinBlank > 0) + nEmptyCell)
    ) # determine the existence of QC and Blank
  finalSeq2 <- finalSeq
  if(positionHolder > 0){finalSeq2 <- finalSeq[-(1:positionHolder)]}

  ## assign sequence position
  df2 <- df2 %>%
    dplyr::mutate(Position = rep(NA, times = nrow(df2)))

  if(nBlank ==0 & nWithinBlank == 0){
    df2 <- df2 %>%
      dplyr::mutate(Position = ifelse(Group == "QC", finalSeq[1], Position))
  } else{
    df2 <- df2 %>%
      dplyr::mutate(Position = ifelse(Group == "Blank", finalSeq[1], Position)) %>%
      dplyr::mutate(Position = ifelse(Group == "QC", finalSeq[2], Position))
  }

  suppressWarnings(df2$Position[is.na(df2$Position)] <- finalSeq2)

  return(df2)
}

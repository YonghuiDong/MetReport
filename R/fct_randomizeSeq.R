#' randomizeSeq
#'
#' @description A fct function
#' @importFrom dplyr %>% row_number add_row
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @examples
#' df <- data.frame(Group = rep(c("A", "B", "C"), each = 20), Rep = rep(1:6, times = 10))
#' result <- randomizeSeq(df, nBlank = 5, nQC = 6)

randomizeSeq <- function(df, nBlank = 0, nQC = 0, plateRow = 6, plateCol = 8, plateIDType = "Letter", outputType = "Orbitrap") {
  ##(1) Suppress the no visible binding for global variable notes
  Group <- n <- Rep <- bin <- Block <- NULL

  ## Best block is designed when the replicates are not the same among different groups
  bestBlock <- df %>%
    dplyr::count(Group) %>%
    dplyr::summarize(Fre = names(which.max(table(n)))) %>%
    as.numeric() %>%
    mean() %>%
    ceiling()

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
    df2 <- df2 %>%
      dplyr::group_by(grp = (row_number() + 1) %/% nQC) %>%
      dplyr::group_modify(~ add_row(.x, Group = rep("QC", 1), Sample = rep("QC", 1))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-grp) %>%
      dplyr::group_by(Sample) %>%
      dplyr::mutate(Sample = ifelse(Sample == "QC", paste0("QC_", (row_number()+1)), Sample)) %>%
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
  df2 <- df2 %>%
    dplyr::mutate(Position = rep(combinations, length.out = nrow(df2)))

  ## add plate ID
  if(plateIDType == "Letter"){repeated_ID = rep(LETTERS[1:26], length.out = nrow(df2))}
  if(plateIDType == "Number"){repeated_ID = rep(1:26, length.out = nrow(df2))}

  df2 <- df2 %>%
    dplyr::mutate(row = row_number(), plateID = repeated_ID[ceiling(row/(plateRow * plateCol))]) %>%
    dplyr::mutate(Position = paste0(plateID, ":", Position)) %>%
    dplyr::select(-c(row, plateID))

  return(df2)
}

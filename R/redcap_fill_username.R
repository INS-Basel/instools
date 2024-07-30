

#' Complete redcap usernames based on csv-filled-lists with names and email addresses
#'
#' @details
#' Admins have the rights to add additional users to REDCap. The PI or data managers of a
#' project is responsible to fill out the user form and send it via Email to the admin.
#' The admin fills out the usernames according to a standardized rule (handle of the email-adress).
#' When bulk-adding multiple users, this is tedious. This function fills the
#' user names automatically and saves the file as a .csv in the same directory as the
#' supplied file.
#'
#'
#' @param which_file as standard REDCap user name list filled out with names and email addresses
#' @param username_from_mail defaults to FALSE. Usernames should be build from First, Last Names. For backward-compatibility.
#'
#' @return a csv-file with suffix _completed.csv
#' @export
#'
redcap_fill_username <- function(which_file = "latest",
                                 username_from_mail = FALSE){

if (which_file == "latest") {

  # check the latest user-list-file available in the directory you are
  last_file <- utils::tail(list.files(pattern = "*.csv"), 1)

  } else {last_file <- which_file}

# import last file
# the na.strings="" is needed to be able to replace only missing usernames
dat <- utils::read.csv(last_file,
                check.names = F,
                na.strings = "",
                encoding = "Latin-1")

# sanitize inputs
dat$`First name` <- stringr::str_squish(dat$`First name`)
dat$`Last name` <- stringr::str_squish(dat$`Last name`)


if (username_from_mail) {
# generate user names from email adresses
dat$Username[is.na(dat$Username)] <- tolower(sub("@.*", "", dat[is.na(dat$Username), "Email address"]))
} else {
  dat$Username[is.na(dat$Username)] <- tolower(paste0(dat$`First name`, ".", dat$`Last name`))
}


# cleanup any possible empty rows in the csv
dat <- dat[!is.na(dat$`First name`), ]

# write new file out
# the na="" character is needed, otherwise redcap complains as it does not
# handle true missing values in the automatic upload
utils::write.csv(dat,
          file = paste0(sub("*.csv", "", last_file), "_filled.csv"),
          row.names = FALSE,
          na = "")

}


#' @seealso [fill_redcap_username()]
#'
complete_redcap_username <- redcap_fill_username

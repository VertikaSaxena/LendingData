## Vertika Saxena
## SBU ID: 111 322 668

library(ggplot2)

File1 <- "./LoanStats_2017Q1.csv"
File2 <- "./LoanStats_2017Q2.csv"
File3 <- "./LoanStats_2017Q3.csv"
File4 <- "./LoanStats_2017Q4.csv"

InputData <- read.csv(file = File1, header = TRUE, sep = ",")
##head(InputData[0])

## Get verified incomes, loan amount, interest, and Loan grade of people
loan_amounts   <- c()
interest_rates <- c()
loan_grades    <- c()
term_length    <- c()
purpose_clean  <- c()
addr_state     <- c()
interest_clean <- c()
loan_amount_clean <- c()
income_clean      <- c()

## Grades are represented from A - G
## but reading them from CSV is giving
## integers. They need to be back converted
## to strings
letters_num <- 65:96
mode(letters_num) <- "raw"
letters <- sapply(letters_num, rawToChar)

i <- 0
for (i in 1 : length(InputData$annual_inc)) {
    loan_amounts    <- c(loan_amounts, InputData$funded_amnt[i])
    interest_rates  <- c(interest_rates, InputData$int_rate[i])
    loan_grades     <- c(loan_grades, InputData$grade[i])
    loanTerm        <- as.numeric(substring(toString(InputData$term[i]), 2, 3))
    term_length     <- c(term_length, loanTerm)
}

## Gradewise bins
gradewise_loan_sum   <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
gradewise_loan_count <- c(0, 0, 0, 0, 0, 0, 0)
gradewise_interest   <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
gradewise_income     <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
gradewise_termlength <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

## Lets make bins after doing some cleaning of data
i <- 0
for (i in 1 : length(loan_grades)) {
  idx <- 0
  ## The grades were getting printed as
  ## integers instead of A,B,C so translating them
  ## A = 2, B = 3 etc.
  if (toString(loan_grades[i]) == "NA") {
    next
  } else {
    idx <- loan_grades[i] - 1
  }
  if ((!is.null(loan_amounts[i])) &&
      (toString(loan_amounts[i]) != "NA") &&
      (toString(interest_rates[i]) != "NA")) {
    # Total loans allocated grade wise
    gradewise_loan_sum[idx] = gradewise_loan_sum[idx] + loan_amounts[i]

    ##Convert interest rate to numeric before using
    rate <- as.double(substring(toString(InputData$int_rate[i]), 0, nchar(toString(InputData$int_rate[i])) - 1))
    gradewise_interest[idx] = gradewise_interest[idx] + rate

    # Gradewise term length and income
    gradewise_termlength[idx] = gradewise_termlength[idx] + term_length[i]
    gradewise_income[idx] = gradewise_income[idx] + InputData$annual_inc[i]
    
    # Only have purposes that are for valid loan amount values
    # Same for interest, income and loan amount
    purpose_clean = c(purpose_clean, toString(InputData$purpose[i]))
    interest_clean = c(interest_clean, InputData$int_rate[i])
    loan_amount_clean = c(loan_amount_clean, InputData$loan_amnt[i])
    income_clean = c(income_clean, InputData$annual_inc[i])
    addr_state = c(addr_state, toString(InputData$addr_state[i]))
    
    # Increment gradewise loan count
    gradewise_loan_count[idx] = gradewise_loan_count[idx] + 1
  }
}


# ============
# Bar charts
# ============
for (i in 1 : 7) {
  gradewise_loan_sum[i]   = (gradewise_loan_sum[i]/gradewise_loan_count[i])
  gradewise_income[i]     = (gradewise_income[i]/gradewise_loan_count[i])
  gradewise_interest[i]   = (gradewise_interest[i]/gradewise_loan_count[i])
  gradewise_termlength[i] = (gradewise_termlength[i]/gradewise_loan_count[i])
}
 
#Plot amount of loan taken by grade
loan_amount_df <- data.frame(Grade_of_borrower = c("A", "B", "C", "D", "E", "F", "G"),
                            Money_lended = gradewise_loan_sum)
ggplot(data=loan_amount_df, aes(x=Grade_of_borrower, y=Money_lended)) + geom_bar(stat="identity")

#Plot income of borrowers by grade
income_df <- data.frame(Grade_of_borrower = c("A", "B", "C", "D", "E", "F", "G"),
                        Borrower_income = gradewise_income)
ggplot(data=income_df, aes(x=Grade_of_borrower, y=Borrower_income)) + geom_bar(stat="identity")
 
#Plot interest rates by grade
interest_df <- data.frame(Grade_of_borrower = c("A", "B", "C", "D", "E", "F", "G"),
                          Interest_rate_offered = gradewise_interest)
ggplot(data=interest_df, aes(x=Grade_of_borrower, y=Interest_rate_offered)) + geom_bar(stat="identity")

#Plot loan term length by grade
term_length_df <- data.frame(Grade_of_borrower = c("A", "B", "C", "D", "E", "F", "G"),
                             Loan_term_length = gradewise_termlength)
ggplot(data=term_length_df, aes(x=Grade_of_borrower, y=Loan_term_length)) + geom_bar(stat="identity")


# ============
# Scatterplots
# ============
letter_grades <- c()
for (i in 1 : length(loan_grades)) {
  letter_grades <- c(letter_grades, letters[loan_grades[i] - 1])
}

## Loan purpose by state
purpose_df <- data.frame(State = addr_state, Purpose = purpose_clean)
ggplot(purpose_df, aes(x=State, y=Purpose)) +
  geom_point(size=1, shape=1)


## Loan purpose by interest rates
purpose_interest_df <- data.frame(Purpose = purpose_clean, Interest = interest_clean)
ggplot(purpose_interest_df, aes(x=Purpose, y=Interest)) +
  geom_point(size=1, shape=1)

## Grade distribution among High income borrowers
grade_income_df <- data.frame(Grade = letter_grades, Income = income_clean)
ggplot(grade_income_df, aes(x=Grade, y=Income)) +
  geom_point(size=1, shape=1) + ylim(100000, 200000)

## Correlation between grades and interest rates
grade_interest_df <- data.frame(Grade = letter_grades, Interest = interest_clean)
ggplot(grade_interest_df, aes(x=Grade, y=Interest)) + 
  geom_point(size=1, shape=1) + ylim(1, 30)

## Incomes and amount borrowed
income_amount_df <- data.frame(Income = income_clean, Amount_borrowed = loan_amount_clean)
ggplot(income_amount_df, aes(x=Income, y=Amount_borrowed)) +
  geom_point(size=1, shape=1) + xlim(50000, 100000)
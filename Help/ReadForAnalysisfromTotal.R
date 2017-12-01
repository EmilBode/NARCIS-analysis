stop('Use of this script is deprecated, instead use ReadForAnalysisfromTotal() from Help.R')

if (!exists('Total')) {Total <- ReadForAnalysisfromTotal()}
TotalVals <- ReadForAnalysisfromTotal(T)
print(LastTotalFilePath)

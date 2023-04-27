import sys

# file_name = sys.argv[1]
num_entries = 999


file_name = "/Users/schmuck/Documents/Box Sync/PhD/battery-machine-intelligence/Results/Zreal_stat.csv"

file_contents = []

table_begin = """
\\begin{table}[!htb]
\\footnotesize
\\centering
\\label{tab:}
\\caption{}
\\begin{adjustbox}{width=\\linewidth}
\\begin{tabular}{lccccc|lccccc|lccccc} 
\\multicolumn{6}{c}{\\textbf{RMSE}} & \\multicolumn{6}{c}{\\textbf{MAE}} & \\multicolumn{6}{c}{\\textbf{RSquared}} \\\ 
\\midrule 
Model & Min & Max & Mean & Median & SD & Model & Min & Max & Mean & Median & SD & Model & Min & Max & Mean & Median & SD \\\ 
\\cmidrule(lr){1-6} \\cmidrule(lr){7-12} \\cmidrule(lr){13-18} \\\ \n
"""

table_content = ""

#### read the file
with open(file_name, "r") as input_file:
  
  lines = input_file.readlines()

  if num_entries != 999:
    lines = lines[1:num_entries]
  
  for line in lines:
    
    line = line.split(",")
    # print("hello", len(line))
    
    for i in range(0, len(line)):
        if i > 0 and i < len(line)-1:
            table_content = table_content + " & " +str(line[i])
        
        elif i == 0:
           table_content = table_content + str(line[i])

        elif i == len(line)-1:
           table_content = table_content + " & " +str(line[i]) + "\\\ \n"    


# table = table_begin + table_content

table_end = """
\\bottomrule
\end{tabular}
\end{adjustbox}
\end{table}
"""

table = table_begin + table_content + table_end

print(table)


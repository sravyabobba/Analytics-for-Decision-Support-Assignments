set numbers;

param Price {i in numbers};
param Prof {i in numbers};

var X {i in numbers} binary;

maximize Total_Profitability: sum {i in numbers} X[i]*Prof[i];

subject to
Budget_Constraint: sum {i in numbers} X[i]*Price[i] <= 10000000;
City_Constraint_Fresno: sum {i in 2..5} X[i] <= 2; 
City_Constraint_Los_Angeles: sum {i in 7..9} X[i] <= 2;
City_Constraint_South_Lake_Tahoe: sum {i in 10..16} X[i] <= 2;

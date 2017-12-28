set numbers;

param Price {i in numbers};
param Prof {i in numbers};

var X {i in numbers} binary;

maximize Total_Profitability: sum {i in numbers} X[i]*Prof[i];

subject to
Budget_Constraint: sum {i in numbers} X[i]*Price[i] <= 10000000;



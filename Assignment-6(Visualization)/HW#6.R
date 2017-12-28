# Homework #6
### Visualization in R ###

Users = read.csv("Users.csv")
str(Users)

Edges = read.csv("Edges.csv")
str(Edges)

#install.packages("ggplot2")
library(ggplot2)

# Answer a(i)
nrow(Users) # Gives [1] 59
# We have 59 Facebook users in our dataset.

# Answer a(ii)
nrow(Edges) # Gives [1] 146
# Now, for each group of users, we have 2 friends on single edge, thus total friends would be 
# 146*2 = 292
# Average number of friends per user would be given by = (total friends/total users)
292/59  # Gives [1] 4.949153 ~ 4.95

# Answer a(iii)
summary(Users) # Shows valid entries for School is either A or AB

# Selecting valid entries dataset as 'Users.School'
Users.School = subset(Users, Users$School == "A" | Users$School == "AB")
table(Users.School$Locale)
# Gives below output
# A  B 
# 0 19 

# Thus, The most common locale out of the students who listed a school is 'Locale B'.

# Answer a(iv)
table(Users$Gender, Users$School)
# Gives
#       A AB
#    1  1  0
# A 11  3  1
# B 28 13  1

# No, neither of school A nor B is all boys or all girls school.
# As we see (from last column named as AB), both genders have attended both the schools (A & B).


# Answer b(i)
#install.packages("igraph")
library(igraph)

g = graph.data.frame(Edges, FALSE, Users)
plot(g, vertex.size = 5, vertex.label=NA)

# From the generated graph, we can see that we have 4 connected components each having at least 2 nodes.
# First connected component is the largest with multiple nodes (44 nodes), two of them have 2 nodes each,
# and the last one is having 4 nodes.

# Answer b(ii)

# From the graph generated in the previous part, we can see that 7 nodes are not connected any other 
# nodes. Hence, 7 users in the network have no friends.

# Answer b(iii)
table(degree(g)) # Gives number of nodes (users, 2nd row) having numberof friends (degree, in 1st row)
# Degree: 0  1  2  3  4  5  6  7  8  9 10 11 13 17 18 
# Nodes:  7 10  4  9  1  4  4  3  6  2  4  1  2  1  1 

table(degree(g) >= 10)
# Gives
# FALSE  TRUE 
# 50     9 

#Thus, 9 users are friends with 10 or more other users in the network.

# Answer c(i)
g = graph.data.frame(Edges, FALSE, Users)
V(g)$color[V(g)$Gender == "A"] = "red"
V(g)$color[V(g)$Gender == "B"] = "gray" 

plot(g, vertex.size = 10, vertex.label=NA)

# After generating the new graph, we can see that majority of users in the network are of gender 'B'. 
# Additionally, 2 people didn't define their gender. Also, we don't see any connection between users to
# be friends with each other based on gender. We can see connection of same color nodes (same gender) as
# well as connections between different color nodes (different genders).

# Answer c(ii)
g = graph.data.frame(Edges, FALSE, Users)
V(g)$color[V(g)$School == "A"] = "red"
V(g)$color[V(g)$School == "AB"] = "gray" 

plot(g, vertex.size = 10, vertex.label=NA)

# We can see from the generated graph that there are only 2 students who attended both the schools A & B,
# (grey color nodes) are Facebook friends with each other.

# Answer c(iii)
g = graph.data.frame(Edges, FALSE, Users)
V(g)$color[V(g)$Locale == "A"] = "red"
V(g)$color[V(g)$Locale == "B"] = "gray" 

plot(g, vertex.size = 10, vertex.label=NA)

# We can see from the generated graph that there exists a connection between the users (nodes) being 
# Facebook friends based on Location. In general, same color nodes are connected.
# Out of 4 connected components, we see that the largely connected component has users from Locale 'B'
# and 4 nodes connected component has all users from Locale 'A'. 
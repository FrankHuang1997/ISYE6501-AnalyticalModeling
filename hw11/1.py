from pulp import *
import pandas as pd
#load data from excel file
df = pd.read_excel('C:/Users/huangchengqi/Desktop/MS SCE/19Fall/ISYE6501/hw11/data 15.2/diet.xls')
data = df.head()
data = df[0:64]
print(data)
#convert data to list
data = data.values.tolist()

foods = [x[0] for x in data]
calories = dict([(x[0], float(x[3])) for x in data])
cholesterol = dict([(x[0], float(x[4])) for x in data])
Fat = dict([(x[0], float(x[5])) for x in data])
sodium = dict([(x[0], float(x[6])) for x in data])
carbohydrates = dict([(x[0], float(x[7])) for x in data])
fiber = dict([(x[0], float(x[8])) for x in data])
protien = dict([(x[0], float(x[9])) for x in data])
v_A = dict([(x[0], float(x[10])) for x in data])
v_C = dict([(x[0], float(x[11])) for x in data])
calcium = dict([(x[0], float(x[12])) for x in data])
iron = dict([(x[0], float(x[13])) for x in data])

# create list for mins and maxes (all foods)
amin = [1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10]
amax = [2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40]


# append collection of contraints for each column 
B = []
for j in range(0,11):
    B.append(dict([(x[0], float(x[j+3])) for x in data]))


# define the cost dictionary
cost = dict([(x[0], float(x[1])) for x in data])


# create the optimization problem framework - minimization problem
problem1 = LpProblem('PuLPTutorial', LpMinimize)


# define the variables - continous
foodVars = LpVariable.dicts("foods", foods,0)


# define the variables - binary
chosenVars = LpVariable.dicts("Chosen",foods,0,1,"Binary")


# dictionary of lp variables 
x = LpVariable.dicts("x", foods, 0)


# define the objective function
problem1 += lpSum([cost[f] * foodVars[f] for f in foods])


# add constraints for all foods
for i in range(0,11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition1 = amin[i] <= + dot_B_x
    problem1 += condition1
    
for i in range(0,11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition2 = amax[i] >= + dot_B_x
    problem1 += condition2

problem1 += lpSum([foodVars[i] for i in foods]) >= 0.1

problem1 += chosenVars['Frozen Broccoli']+chosenVars['Celery, Raw'] <= 1

problem1 += chosenVars['Roasted Chicken']+chosenVars['Poached Eggs']+chosenVars['Scrambled Eggs']+chosenVars['Bologna,Turkey']+chosenVars['Frankfurter, Beef']+chosenVars['Ham,Sliced,Extralean']+chosenVars['Kielbasa,Prk']+chosenVars['Pizza W/Pepperoni']+chosenVars['Pork']+chosenVars['White Tuna in Water']+chosenVars['Chicknoodl Soup']+chosenVars['Splt Pea&Hamsoup']+chosenVars['Vegetbeef Soup']+chosenVars['Beanbacn Soup,W/Watr'] >= 3

# solve the optimization problem
problem1.solve()


# print the foods of the optimal diet
print('Optimization Solution:')
for var in problem1.variables():
    if var.varValue > 0:
        if str(var).find('Chosen'):
            print(str(var.varValue) + " units of " + str(var))
            
# print the costs of the optimal diet             
print("Total cost of food = $%.2f" % value(problem1.objective))

print(Chosen[0])
















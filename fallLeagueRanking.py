from numpy import matrix
teamList = ["Piehole", "Ghetto Birds", "Whiskey Tango Foxtrot", "Eats, Throws, and Leaves", "Jazz Picnic", "Toolbox", "Blue Footed Boobies"]
print(teamList)
len(teamList)
winMatrix = matrix([0,1,1,1,1,1,1],[0,0,1,1,1,1,1],[0,0,0,0,1,1,1],[0,0,1,0,0,1,1],[1,0,0,1,0,1,0],[0,0,0,0,0,0,1],[0,0,0,0,1,0,0])
print(winMatrix)
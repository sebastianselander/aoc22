file = open("input.txt","r")


lines = file.read().split('\n')

for l in lines:
    l + "HEJ"
    print(l)





file.close()

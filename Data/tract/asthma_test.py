import os

x = os.listdir('.')

for file in x:
	if '.RData' in file and os.path.isfile(file):
		state = file.split('.RData')[0].split('_')[-1]
		if not os.path.exists(state):
			os.mkdir(state)
		os.rename(file,state+'/'+file)

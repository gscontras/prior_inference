#!/usr/bin/env python
# -*- coding: utf-8 -*-

#(red / blue / green), a shape (circle / square / cloud), 
# and a texture (solid / polka-dot / striped)


'''
Script to generate stimuli

Note: for "prior inference", "target" is what the listener picks
For mutual understanding, "target" is what the speaker wants to point 
out and "listenerpick" is what the listener picks...

'''


from tqdm import tqdm
import random
from collections import Counter, defaultdict
import json
import re

shapes = {1:'cloud',2:'circle',3:'square'}
colors = {1:'blue',2:'red',3:'green'}
textures = {1:'solid',2:'striped',3:'polka-dotted'}
features = {1:"shape", 2:'texture', 3:'color'}
featuresback = {"shape":1, 'texture':2, 'color':3}

l = [1,2,3]

martinscode2ninecode = {1:"111",2:"211",3:"311",4:"121",5:"221",6:"321",
				7:"131",8:"231",9:"331",10:"112",11:"212",12:"312",
				13:"122",14:"222",15:"322",16:"132",17:"232",18:"332",
				19:"113",20:"213",21:"313",22:"123",23:"223",24:"323",
				25:"133",26:"233",27:"333"}
ninecode2martinscode = {v:k for k,v in martinscode2ninecode.items()}

'''
#
# Provide read / write / basic infos functions
#
'''
def getFeatureName(feature, value):
	if feature == 1:
		return shapes[value]
	if feature == 2:
		return textures[value]
	if feature == 3:
		return colors[value]
	raise ValueError


def mywrite(stimuli,basefilename):
	with open(basefilename+".json", "w") as fp:
		json.dump(stimuli, fp, indent=2)
	filename = basefilename + ".js"
	with open(filename,'w') as fp:
		fp.write("stimulidata = ")
	with open(filename,'a') as fp:
		json.dump(stimuli, fp, indent=2)
		fp.write(";")
	with open(filename, "r") as fp:
		t = fp.read()
		t = re.sub("([^}]{2})(\\n)","\\1",t)
		t = re.sub('[ ]+', '', t)
		t = re.sub('\n', '\n\t', t)
		t = re.sub('''"([\w-]+)":''', "\\1:", t)
	with open(filename, "w") as fp:
		fp.write(t)

'''
#
# Go through the pipeline
#
'''


'''
First step: compute list of possible stimuli
'''
def create_sl_all():
	stimulilist = [[x,y,z] for z in range(1,28) for y in range(1,28) 
									for x in range(1,28) if x<y and y<z]
	return stimulilist

l = create_sl_all()

print(len(l))

'''
Second step (optional ; ): remove cases with duplicate objects
'''
#done in step one : )

'''
Third step: add utterance choice code because we dont have utterance yet
'''

def getsharedinfo(item):
	s = dict()
	objnums = {0:0, 1:0, 2:0}
	for i in range(3):
		# info needed: num of most shared, which share
		num = 4 - len(set([x[i] for x in item]))
		ind = []
		if num == 2:
			f = item[0][i]
			if item[1][i] == f:
				ind = [0,1]
			elif item[2][i] == f:
				ind = [0,2]
			else:
				ind = [1,2]
		if num == 3: 
			# useless but good for understanding this mess...
			ind = [0,1,2]
		s[i] = {"num":num,"objects":ind}
		for j in ind:
			objnums[j] += 1
	for i in range(3):#weight by whether the 
		if s[i]["num"] == 3:
			s[i]["weight"] = 15
		if s[i]["num"] == 2:
			s[i]["weight"] = 0
			for ind in s[i]["objects"]:
				s[i]["weight"] += objnums[ind]
			s[i]["weight"] *= 2
		if s[i]["num"] == 1:
			s[i]["weight"] = 1
	return [s[x] for x in s]

def computeUtteranceChoiceCode(item):
	obj1 = None
	obj2 = None
	si = getsharedinfo(item)
	si = sorted(si, key=lambda x: x["weight"], reverse = True)
	code = ""
	for i in range(3):
		code += str(si[i]["num"])
		if si[i]["num"] == 2:
			if obj1 == None:
				obj1 = si[i]["objects"][0]
				obj2 = si[i]["objects"][1]
			else:
				if obj1 == si[i]["objects"][0] and obj2 == si[i]["objects"][1]:
					code += "a"
				else:
					code += "b"
	if code == "22b2a":
		code = "22a2b"
		print("why did 22b2a still happen...")
	return code
		
	

def addUtteranceChoiceCode(stimuli):
	t = lambda x : martinscode2ninecode[x]
	fullstimuli = [[t(x[0]),t(x[1]),t(x[2])] for x in stimuli]
	codeadded = [{"item":x,"utterancecode":computeUtteranceChoiceCode(x)} for x in fullstimuli]
	return codeadded

l = addUtteranceChoiceCode(l)

def addIDs(stimuli):
	for i in range(len(stimuli)):
		stimuli[i]["IDf"] = '{0:04d}'.format(i)
		stimuli[i]["ID"] = i
	return stimuli
	
l = addIDs(l)

#[{"ID":id, "item":[i1,i2,i3],"utterancecode":utterancecode}]


'''
Step 3.1: Add a target feature for getting preference information. 
  The number that represents that feature in the uttchoicecode is added
  with a hyphen at the end.
'''


def addPrefTarget(stimuli):
	newlist = []
	for s in stimuli:
		item = s["item"]
		idaddition = 0
		for j in range(3):#targetfeature
			num = 4 - len(set([x[j] for x in item]))
			if num < 3:
				news = s.copy()
				news["ID"] = int(str(news["ID"]) + str(idaddition))
				news["IDf"] += "-" + str(idaddition)
				idaddition += 1
				f = features[j+1]
				news["targetfeature"] = f
				news["targetfeaturenum"] = j+1
				if news["utterancecode"] != "22a2b":
					news["targeteduttcode"] = news["utterancecode"] + "-" + str(4-num)
				else:
					v = item[0][j]
					if item[1][j] != v and item[2][j] != v:
						v = item[1][j]
					tmp = [x for x in item if x[j] == v]
					tmp2 = [x for x in range(3) if x != j]
					if tmp[0][tmp2[0]] == tmp[1][tmp2[0]] or tmp[0][tmp2[1]] == tmp[1][tmp2[1]]:
						news["targeteduttcode"] = news["utterancecode"] + "-2a"
					else:
						news["targeteduttcode"] = news["utterancecode"] + "-2b"
				newlist.append(news)
	return newlist

l = addPrefTarget(l)		

def computeInferencePossibilities(stimulus):
	f = featuresback[stimulus["targetfeature"]]
	choiceslist = dict()
	for i in [x for x in range(3) if x != f-1]: #features
		for j in range(3): # values
			referenceditems = []
			for k in range(3): # items
				if stimulus["item"][k][i] == str(j+1):
					#the stimulus has, as the currently looked at feature,
					#  the currently looked at value (utterance), i.e.
					#  with using that utterance we can reference it.
					referenceditems.append(k)
			#now we have to look how many different values for the
			#  target feature we get in those items
			num = len(set([stimulus["item"][x][f-1] for x in referenceditems]))
			if num > 0:
				choiceslist[getFeatureName(i+1, j+1)] = num
	# ~ return "".join([str(x) for x in sorted(choiceslist,reverse=True)])
	return choiceslist

# add numFeatures and featuresPresent
# NOTE: This works for the targeted UttChoice, but the values would 
#    later be overwritten by the numFeatures overall!!!
def addTargetedUttPossibleFeatures(stimuli):
	for i in range(len(stimuli)):
		choiceslist = computeInferencePossibilities(stimuli[i])
		stimuli[i]["numFeatures"] = len(choiceslist)
		stimuli[i]["featuresPresent"] = list(choiceslist.keys())
	return stimuli

l = addTargetedUttPossibleFeatures(l)

#DEBUG: print list with items, unique by targeteduttcode and how much 
#  inference each utterance will allow
#  This is currently BROKEN!
# ~ def getUniqueListOfTargetedUttCode(stimuli):
	# ~ print("DEBUG: computing unique list of targeted utt codes")
	# ~ s = set()
	# ~ checks = set()
	# ~ uniquelist = []
	# ~ for i in stimuli:
		# ~ choices = computeInferencePossibilities(i)
		# ~ identifier = "--".join([i["targeteduttcode"], choices])
		# ~ if identifier not in s:
			# ~ s.add(identifier)
			# ~ i["choiceslist"] = choices
			# ~ uniquelist.append(i)
		# ~ if i["targeteduttcode"] not in checks:
			# ~ checks.add(i["targeteduttcode"])
	# ~ if len(s) != len(checks):
		# ~ print("ERROR: codes are not unique to choices")
	# ~ filename = "uniquelistofUttCodes.json"
	# ~ with open(filename,"w") as fn:
		# ~ json.dump(uniquelist, fn, indent=2)
	# ~ with open(filename, "r") as fp:
		# ~ t = fp.read()
		# ~ t = re.sub("([^}]{2})(\\n)","\\1",t)
		# ~ t = re.sub('[ ]+', '', t)
		# ~ t = re.sub('\n', '\n\t', t)
		# ~ t = re.sub('''"([\w-]+)":''', "\\1:", t)
	# ~ with open(filename, "w") as fp:
		# ~ fp.write(t)

# ~ getUniqueListOfTargetedUttCode(l)
# ~ print("exiting after computing unique list, comment out to change")

BINSIZE = 50

def produceTargetedUttBins(stimuli):
	bins = defaultdict(list)
	shuffled = random.sample(stimuli, len(stimuli))
	tofilllists = set([x["targeteduttcode"] for x in stimuli])
	t = tqdm(total=len(tofilllists))
	while (len(tofilllists) > 0 and len(shuffled) > 0):
		i = shuffled.pop()
		code = i["targeteduttcode"]
		if len(bins[code]) < BINSIZE:
			#check for siblings
			family = [x for x in stimuli if x["targeteduttcode"] == code and x["IDf"][:-1] == i["IDf"][:-1] and x["IDf"] != i["IDf"]]
			#add to bin
			bins[code].append(i)
			while len(bins[code]) < BINSIZE and len(family) > 0:
				bins[code].append(family.pop())
		elif (code in tofilllists):
			tofilllists.remove(code)
			t.update()
	t.close()
	for k in bins:
		mywrite(bins[k],"targeteduttsstimuli/"+k)
	fulllist = []
	for k,v in bins.items():
		fulllist += v
	mywrite(fulllist, "targeteduttsstimuli/50ByTargetedUtt")
		
produceTargetedUttBins(l)

print("exiting after producing targeted utterance code bins")
exit()


'''
Fourth step: add utterance and listener choice (This of course makes the list muuuch longer)
'''

def addUtterances(stimuli):
	newlist = []
	for stimulus in stimuli:
		item = stimulus["item"]
		idaddition = 0
		for i in range(len(item)):#target
			s = item[i]
			for j in range(3):#cycle through features
				# ~ for k in range(3):#cycle through listener picks
					# ~ if item[k][j] == item[i][j]:
				newstimulus = stimulus.copy()
				newstimulus["targetind"] = i
				newstimulus["utterance"] = getFeatureName(j+1,int(s[j]))
				newstimulus["utteredtype"] = j+1
				# ~ newstimulus["listenerpick"] = k
				newstimulus["ID"] = int(str(newstimulus["ID"]) + str(idaddition))
				newstimulus["IDf"] += "-" + str(idaddition)
				idaddition += 1
				newlist.append(newstimulus)
	return newlist

l = addUtterances(l)
'''
[{"item":[i1,i2,i3],"utterancecode":utterancecode, "targetind": i, 
	"utterance":uttr, "utteredtype": numtype, "listenerpick": i}]
'''
'''
Fifth step: compute "ambiguity code" (prior inference)
'''

class AmbiguityCodeLong():
	'''
	class-description
	'''
	def __init__(self):
		pass
		
	def _getNumOfFeature(self, items, feature, index):
		count = 0
		index = str(index)
		featurelist = [str(x)[feature-1] for x in items]
		for i in featurelist:
			if i == index:
				count += 1
		if count == 1:
			featurelist = [x for x in featurelist if x != index]
			if featurelist[0] == featurelist[1]:
				return "12"
			else: 
				return "11"
		return str(count)

	def _getIdentical(self, obj2, obj3, feature, index):
		obj2 = str(obj2)
		obj3 = str(obj3)
		index = str(index)
		if obj2[feature-1] == index:
			return 'obj2'
		else: 
			return 'obj3'

	def _buildCode(self, numberdict):
		#builds the 6-digit ambiguity code
		code = [numberdict["utteredf"]]
		otherfeatures = [x for x in numberdict.keys() if x != "utteredf"]
		featuresequence = []
		k = dict()
		for f in otherfeatures:
			k[f] = 0 #11 is fifth
			num = numberdict[f]
			if num == "32":#first
				k[f] += 4
			elif num == "12":#fourth
				k[f] += 1
			elif num.startswith("2"):
				if num == "21":#second
					k[f] += 3
				elif num == "22":#third
					k[f] += 2
		if k[otherfeatures[0]] > k[otherfeatures[1]]:
			code.append(numberdict[otherfeatures[0]])
			featuresequence.append(otherfeatures[0])
			code.append(numberdict[otherfeatures[1]])
			featuresequence.append(otherfeatures[1])
		else:
			code.append(numberdict[otherfeatures[1]])
			featuresequence.append(otherfeatures[1])
			code.append(numberdict[otherfeatures[0]])
			featuresequence.append(otherfeatures[0])
		return {"code" :code, "codepos2featnum": featuresequence[0], 
						"codepos3featnum": featuresequence[1]}

	def _determineCode(self, entry):
		#stimulus: dict[targetind, obj2, obj3, kepttype, contexttype, utteredtype]
		second = None
		items = entry['item']
		target = entry["targetind"]
		rest = [x for x in range(3) if x != target]
		obj2 = rest[0]
		obj3 = rest[1]
		utfeature = entry['utteredtype']
		utvalue = items[target][utfeature-1]
		firstnumber = self._getNumOfFeature(items, utfeature, utvalue)
		#print(firstnumber)
		if firstnumber == "3":
			firstnumber = "32"
		if firstnumber == "2":
			second = self._getIdentical(items[obj2], items[obj3], utfeature, utvalue)
			firstnumber += "1"
			# second sets the second object and is either obj2 or obj3
			
		otherfeatures = [i for i in range(1,4) if i != utfeature]
		ofs = {"utteredf":firstnumber}
		for f in otherfeatures:
			index = str(items[target])[f-1]
			ofnum = self._getNumOfFeature(items, f, index)
			if ofnum == "3":
				ofnum += "2"
			if ofnum == "2":
				ident = self._getIdentical(items[obj2], items[obj3], f, index)
				if second == None:
					second = ident
					ofnum += "1"
				else:
					if ident == second:
						ofnum += "1"
					else: 
						ofnum += "2"
			ofs[f] = ofnum
			
		#print(ofs)
		switch = False
		if second == "obj3":
			#not necessary, just to show that we did the switch here
			switch = True
			tmp = obj2
			obj2 = obj3
			obj3 = tmp
		
		return self._buildCode(ofs), obj2,obj3
		
	def formatStimulus(self, stimulus, ID):
		#stimulus: dict[target, obj2, obj3, contexttype, utteredtype]
		assert type(ID) == str
		#otherfeatures = [x for x in l if x != stimulus["kepttype"]]
		strout = '{ID:' + ID + ',target:"' + formatItem(stimulus["target"])+'",'
		strout += 'obj2:"' + formatItem(stimulus["obj2"])+'",'
		strout += 'obj3:"' + formatItem(stimulus["obj3"])+'",'
		#stimulus += 'condition:"' + condition + '", flag:"' + flag + '"},\n'
		strout += 'itemCode:"' + stimulus["contexttype"]
		#strout += '",property1:"' + features[otherfeatures[0]]
		#strout += '",property2:"' + features[otherfeatures[1]]
		strout += '",targetShape:"' + shapes[int(stimulus["target"][0])]
		strout += '",targetTexture:"' + textures[int(stimulus["target"][1])]
		strout += '",targetColor:"' + colors[int(stimulus["target"][2])]
		utterance = getFeatureName(stimulus["utteredtype"], int(stimulus["target"][stimulus["utteredtype"]-1]))
		strout += '",utterance:"' + utterance
		strout += '",utteredtype:"' + features[stimulus["utteredtype"]]
		numfeatures, presentfeatures = self._getStimulusFeatures(stimulus)
		strout += '",numFeatures:"' + str(numfeatures)
		strout += '",featuresPresent:"' + str(presentfeatures)
		learnnothing = stimulus["contexttype"] in ["323232","213232","212121","212132"]
		learnnothing = learnnothing or stimulus["contexttype"].startswith("1")
		strout += '",listener-ambiguous:"' + str((not learnnothing))
		strout += '",speaker-ambiguous:"' + str(not (stimulus["contexttype"] in ["323232","111111","212121","121212"]))
		strout += '"},\n'
		return strout

def addAmbiguityCode(stimuli):
	AC = AmbiguityCodeLong()
	for i in range(len(stimuli)):
		codedict, obj2ind, obj3ind = AC._determineCode(stimuli[i])
		stimuli[i]["priorInfCode"] = "".join(codedict["code"])
		stimuli[i]["obj2"] = stimuli[i]["item"][obj2ind]
		stimuli[i]["obj3"] = stimuli[i]["item"][obj3ind]
		stimuli[i]["obj2ind"] = obj2ind
		stimuli[i]["obj3ind"] = obj3ind
	return stimuli

l = addAmbiguityCode(l)

'''
[{"item":[i1,i2,i3],"utterancecode":utterancecode, "targetind": i, 
	"utterance":uttr, "utteredtype": numtype, 
	"priorInfCode":code}]
'''

'''
add "listenerpick": i,
before, target was what the listener picked - now, the listener picks
'''

def addListenerpick(stimuli):
	newlist = []
	for stimulus in stimuli:
		item = stimulus["item"]
		idaddition = 0
		for k in range(3):#cycle through listener picks
			if item[k][stimulus["utteredtype"]-1] == item[stimulus["targetind"]][stimulus["utteredtype"]-1]:
				newstimulus = stimulus.copy()
				newstimulus["listenerpick"] = k
				newstimulus["ID"] = int(str(newstimulus["ID"]) + str(idaddition))
				newstimulus["IDf"] += "-" + str(idaddition)
				idaddition += 1
				newlist.append(newstimulus)
	return newlist

l = addListenerpick(l)

'''
Sixth step: compute mutual understanding code
'''
def addMutualUnderstandingCode(stimuli):
	for i in range(len(stimuli)):
		s = stimuli[i]
		codedict = code2understandingtype([
					s["item"][s["targetind"]],
					s["obj2"],#[[x for x in range(3) if x != s["targetind"]][0]],
					s["obj3"],#[[x for x in range(3) if x != s["targetind"]][1]],
					s["utteredtype"],
					s["item"][s["listenerpick"]] ] )
		stimuli[i].update(codedict)
	return stimuli

#[targetind,o1,o2,utteredfeature,listenerpick]
def code2understandingtype(stimulus):
	responsedict = {"MUnumchoices": 0, "MUalign":None, "MUsharedlistenerpick":"NA", "MUsharedother":"NA", "MUcode": ""}
	f = stimulus[3]-1
	item = stimulus[:3]
	lt = stimulus[4] #listenerpick
	num = len([int(x[f]) for x in item if x[f] == item[0][f]])
	responsedict["MUnumchoices"] = num
	if lt == item[0]:
		responsedict["MUalign"] = "same"
	else:
		responsedict["MUalign"] = "diff"
		responsedict["MUsharedlistenerpick"] = entitysimilarity(item[0],lt)
		if num > 2:
			if lt == item[1]:
				simo = entitysimilarity(item[2],item[0])
			else:
				simo = entitysimilarity(item[1],item[0])
			responsedict["MUsharedother"] = simo
	responsedict["MUcode"] = str(num)+ responsedict["MUalign"]
	responsedict["MUcode"] += str(responsedict["MUsharedlistenerpick"]) 
	responsedict["MUcode"] += str(responsedict["MUsharedother"])
	# ~ if num == 1:
		# ~ return "unique"
	# ~ if num == 2:
		# ~ if lt == item[0]:
			# ~ return "bi-align"
		# ~ if entitysimilarity(item[0], lt) == 2:
			# ~ return "bi-close"
		# ~ else:
			# ~ return "bi-far"
	# ~ if num == 3:
		# ~ if lt == item[0]:
			# ~ return "tri-align"
		# ~ simt = entitysimilarity(item[0],lt)
		# ~ if lt == item[1]:
			# ~ simo = entitysimilarity(item[2],item[0])
		# ~ else:
			# ~ simo = entitysimilarity(item[1],item[0])
		# ~ if simt == 2:
			# ~ if simo == 2:
				# ~ return "tri-close-close"
			# ~ else:
				# ~ return "tri-close-far"
		# ~ else:
			# ~ if simo == 2:
				# ~ return "tri-far-close"
			# ~ else:
				# ~ return "tri-far-far"
	return responsedict

def entitysimilarity(ent1, ent2):
	sim = 0
	for i in [0,1,2]:
		if ent1[i] == ent2[i]:
			sim += 1
	return sim

def writestimulilist(d, fp):
	ID = 0
	helper = {0:"speakertarget",1:"obj2",2:"obj3"}
	for k in d:
		t = d[k]
		k = k.split(",")
		strout = '{ID:' + str(ID) + ',speakertarget:"' + k[0] +'",'
		strout += 'obj2:"' + k[1] +'",'
		strout += 'obj3:"' + k[2] +'",'
		utteredfeature = int(k[3])
		utterance = getFeatureName(utteredfeature, int(k[0][utteredfeature-1]))
		strout += 'listenertarget:"' + helper[int(k[4])]+'",'
		strout += 'utterance:"' + utterance+'",'
		#strout += '",utteredtype:"' + features[utteredfeature]
		strout += 'stimulustype:"' + t + '"},\n'
		fp.write(strout)
		ID += 1

def choosebytype(d):
	values = set(d.values())
	d2 = {v:[] for v in values}
	for k,v in d.items():
		d2[v].append(k)
	return d2

l = addMutualUnderstandingCode(l)

'''
[{"item":[i1,i2,i3],"utterancecode":utterancecode, "targetind": i, 
	"utterance":uttr, "utteredtype": numtype, "listenerpick": i,
	"priorInfCode":code, "MUnumchoices": n, "MUalign":None, 
	"MUsharedlistenerpick":"NA", "MUsharedother":"NA"}]
	
	
{target:"333",
obj2:"311",
obj3:"111",
targetShape:"square",
targetTexture:"polka-dotted",
targetColor:"green",
listenerAmbiguous:"False",
speakerAmbiguous:"True”}
'''

'''
Sixth step: add further fields
'''

# ~ def addIDs(stimuli):
	# ~ for i in range(len(stimuli)):
		# ~ stimuli[i]["ID"] = i
	# ~ return stimuli
	
# ~ l = addIDs(l)

def getStimulusFeatures(items):
	features = set()
	for i in items:
		for j in range(1,4):
			features.add(getFeatureName(j, int(i[j-1])))
	return len(list(features)), list(features)

def addFeaturesPresent(stimuli):
	for i in range(len(stimuli)):
		numFeatures, featuresPresent = getStimulusFeatures(stimuli[i]["item"])
		stimuli[i]["numFeatures"] = numFeatures
		stimuli[i]["featuresPresent"] = featuresPresent
	return stimuli

l = addFeaturesPresent(l)

def addTargetEtc(stimuli):
	for i in range(len(stimuli)):
		s = stimuli[i]
		t = s["item"][s["targetind"]]
		stimuli[i]["target"] = t
		stimuli[i]["targetShape"] = shapes[int(t[0])]
		stimuli[i]["targetTexture"] = textures[int(t[1])]
		stimuli[i]["targetColor"] = colors[int(t[2])]
	return stimuli

l = addTargetEtc(l)

def addAmbiguous(stimuli):
	for i in range(len(stimuli)):
		s = stimuli[i]
		learnnothing = s["priorInfCode"] in ["323232","213232","212121","212132"]
		learnnothing = learnnothing or s["priorInfCode"].startswith("1")
		stimuli[i]["listener-ambiguous"] = str((not learnnothing))
		stimuli[i]["speaker-ambiguous"] = str(not (s["priorInfCode"] in ["323232","111111","212121","121212"]))
	return stimuli

l = addAmbiguous(l)

'''
7th step: produce output files
'''




'''
Create samples:
Each sample should: 
	how many
	define which are the "categories"
	how many of each
	what to include in the output
'''
# ~ l2 = []
# ~ s = random.sample(range(len(l)), 10)
# ~ for i in s:
	# ~ l2.append(l[i])
# ~ mywrite(l2, "test")

# ~ print("writing all")
# ~ mywrite(l, "allstimuli")

def createandwriteSamples(stimuli, category, numpcat, collapseutt, trialname):
	trials = []
	binnedstimuli = defaultdict(list)
	if collapseutt:
		binnedcollapsesets = defaultdict(set)
	for s in stimuli:
		if collapseutt:
			if s["IDf"][:4] in binnedcollapsesets[s[category]]:
				continue
			else:
				binnedcollapsesets[s[category]].add(s["IDf"][:4])
				binnedstimuli[s[category]].append(s)
		else:
			binnedstimuli[s[category]].append(s)
	for c in binnedstimuli:
		if len(binnedstimuli[c]) < numpcat:
			print(c)
			print(len(binnedstimuli[c]))
			sample = range(len(binnedstimuli[c]))
		else:
			sample = random.sample(range(len(binnedstimuli[c])), numpcat)
		for j in sample:
			trials.append(binnedstimuli[c][j])
	
	mywrite(trials, trialname)
	
print("writing 50ByUtteranceCode")
createandwriteSamples(l, "utterancecode", 50, True, "50ByUtteranceCode")
print("writing 50BypriorInfCode")
createandwriteSamples(l, "priorInfCode", 50, False, "50BypriorInfCode")
print("writing 50ByMUcode")
createandwriteSamples(l, "MUcode", 50, False, "50ByMUcode")
		

'''
DEBUG section ; )
'''

# ~ m = set()
# ~ tmp = []
# ~ for s in l:
	# ~ if s["utterancecode"] == "331":
		# ~ if s["IDf"][:4] in m:
			# ~ continue
		# ~ else:
			# ~ m.add(s["IDf"][:4])
			# ~ tmp.append(s["item"])
# ~ with open("debug.txt", "w") as fp:
	# ~ for i in tmp:
		# ~ fp.write(",".join(i))
		# ~ fp.write("\n")

# ~ with open("50BypriorInfCode.json") as fn: 
	# ~ data = json.load(fn)

# ~ def convertitem(i): 
    # ~ return "-".join([shapes[int(i[0])], textures[int(i[1])], colors[int(i[2])]])

# ~ examples = dict()

# ~ for i in data: 
	# ~ if i["priorInfCode"] in examples: 
		# ~ continue 
	# ~ examples[i["priorInfCode"]] = {
			# ~ "target":convertitem(i["item"][i["targetind"]]), 
			# ~ "obj2":convertitem(i["item"][i["obj2ind"]]), 
			# ~ "obj3":convertitem(i["item"][i["obj3ind"]]),
			# ~ "utterance":i["utterance"],
			# ~ "utterancecode":i["utterancecode"]} 

# ~ df = pd.read_csv("bigTablePostListPrefs_20181206.csv") 

# ~ exp = pd.read_csv("AmbiguityCodeGlossary.csv", index_col=0)

# ~ for code, values in examples.items(): 
	# ~ for k in ["target","obj2","obj3", "utterance", "utterancecode"]: 
		# ~ exp.loc[int(code),k] = values[k]

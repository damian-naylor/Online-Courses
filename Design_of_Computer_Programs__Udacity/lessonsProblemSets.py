# Design of Computer Programs - Udacity

## LESSON 1 ##

# Problem Set
#Q1
def best_hand(hand):
	return max(itertools.combinations(hand, 5), key=hand_rank)
	
#Q2
allranks = '23456789TJQKA'
redcards = [r+s for r in allranks for s in 'DH']
blackcards = [r+s for r in allranks for s in 'SC']

def best_wild_hand(hand):
	"""Try all values for jokers in all 5 card selections"""
	hands = set(best_hand(h) for h in itertools.product(*map(replacements, hand)))
	return max(hands, key=hand_rank)
	
def replacements(card):
	"""Return a list containing the card itself for normal cards or,
	a list of all the possible replacements for a wildcard"""
	if cards =='?B': return blackcards
	elif card =='?R': return redcards
	else: return [card]


## LESSON 2 ##

# Problem Set
#Q1

	
	
	
	
	
	
	
	
	
	
	
	
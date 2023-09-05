# AINE-AI
import random
possible_actions = ["rock", "paper", "scissors", "lizard", "spock"]

while True:
    user_action = input("Enter a choice (rock, paper, scissors, lizard, spock), Enter 'Q' to quit the game: ")
    if user_action.isdigit():
        print('invalid input')
        break
    user_action = user_action.lower()
    if user_action == "q":
        break
    if not user_action in possible_actions:
        print('invalid input')
        break
        
    computer_action = random.choice(possible_actions)
    print(f"\nYou chose {user_action}, computer chose {computer_action}. \n")
    if user_action == computer_action:
        print(f"Both players selected {user_action}. It's a tie!")
    elif user_action == "rock":
        if computer_action == "scissors":
            print("Rock crushes scissors! You win.")
        elif computer_action == "paper":
            print("Paper covers rock! You lose")
        elif computer_action == "lizard":
            print("Rock crushes lizard! You win")
        else:
            print("Spock vaporizes rock! You lose")
    elif user_action == "paper":
        if computer_action == "rock":
            print("Paper covers rock! You win")
        elif computer_action == "scissors":
            print("scissors cuts paper! You lose")
        elif computer_action == "lizard":
            print("lizard eats paper! You lose")
        else:
            print("paper disproves spock! You win")
    elif user_action == "scissors":
        if computer_action == "paper":
            print("scissors cuts paper! You win")
        elif computer_action == "rock":
            print("rock crushes scissors! You lose")
        elif computer_action == "lizard":
            print("scissors decapitates lizard! You win")
        else:
            print("spock smashes scissors! You lose")
    elif user_action == "lizard":
        if computer_action == "paper":
            print("lizard eats paper! You win")
        elif computer_action == "rock":
            print("rock crushes lizard! You lose")
        elif computer_action == "scissors":
            print("scissors decapitates lizard! You lose")
        else:
            print("lizard poisons spock! You win")
    elif user_action == "spock":
        if computer_action == "paper":
            print("paper disproves spock! You lose")
        elif computer_action == "rock":
            print(" spock vaporizes rock! You win")
        elif computer_action == "scissors":
            print("spock smashes scissors! You win")
        else:
            print("lizard poisons spock! You lose")
    else:
        print("invalid input")
    print("......................")
    
    
    import random
import sys

rounds=input("Enter number of rounds:")
possible_actions = ["rock", "paper", "scissors", "lizard", "spock"]
if not rounds.isdigit():
    print("invalid input")
    sys.exit()
    
for i in range(int(rounds)):
    user_action = input("Enter a choice (rock, paper, scissors, lizard, spock): ")
    if user_action.isdigit():
        print('invalid input')
        break
    user_action = user_action.lower()
    
    
    if not user_action in possible_actions:
        print('invalid input')
        break
        
    computer_action = random.choice(possible_actions)
    print(f"\nYou chose {user_action}, computer chose {computer_action}. \n")
    if user_action == computer_action:
        print(f"Both players selected {user_action}. It's a tie!")
    elif user_action == "rock":
        if computer_action == "scissors":
            print("Rock crushes scissors! You win.")
        elif computer_action == "paper":
            print("Paper covers rock! You lose")
        elif computer_action == "lizard":
            print("Rock crushes lizard! You win")
        else:
            print("Spock vaporizes rock! You lose")
    elif user_action == "paper":
        if computer_action == "rock":
            print("Paper covers rock! You win")
        elif computer_action == "scissors":
            print("scissors cuts paper! You lose")
        elif computer_action == "lizard":
            print("lizard eats paper! You lose")
        else:
            print("paper disproves spock! You win")
    elif user_action == "scissors":
        if computer_action == "paper":
            print("scissors cuts paper! You win")
        elif computer_action == "rock":
            print("rock crushes scissors! You lose")
        elif computer_action == "lizard":
            print("scissors decapitates lizard! You win")
        else:
            print("spock smashes scissors! You lose")
    elif user_action == "lizard":
        if computer_action == "paper":
            print("lizard eats paper! You win")
        elif computer_action == "rock":
            print("rock crushes lizard! You lose")
        elif computer_action == "scissors":
            print("scissors decapitates lizard! You lose")
        else:
            print("lizard poisons spock! You win")
    elif user_action == "spock":
        if computer_action == "paper":
            print("paper disproves spock! You lose")
        elif computer_action == "rock":
            print(" spock vaporizes rock! You win")
        elif computer_action == "scissors":
            print("spock smashes scissors! You win")
        else:
            print("lizard poisons spock! You lose")
    else:
        print("invalid input")
    print("......................")

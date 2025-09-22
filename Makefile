##
## EPITECH PROJECT, 2025
## makefile glados
## File description:
## Makefile for glados
##

NAME	= glados

STACKNAME	=	glados-exe

all:	$(NAME)

$(NAME):
	stack build --copy-bins --allow-different-user
	mv $(STACKNAME)	$@

clean:
	stack clean
	$(RM) test/coverage/*.html
	$(RM) test/coverage/*.tix

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

tests_run:
	stack test --coverage --allow-different-user
	stack hpc report --all --destdir ./test/coverage

.PHONY: all clear fclean re tests_run

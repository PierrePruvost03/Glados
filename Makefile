##
## EPITECH PROJECT, 2025
## makefile glados
## File description:
## Makefile for glados
##

all: lisp kong

lisp:
	@echo "=== Building LISP Interpreter ==="
	cd LispInterpreter && $(MAKE)

kong:
	@echo "=== Building Kong Compiler ==="
	cd Kong && $(MAKE)

test: test-lisp test-kong

test-lisp:
	@echo "=== Testing LISP Interpreter ==="
	cd LispInterpreter && $(MAKE) test

test-kong:
	@echo "=== Testing Kong Compiler ==="
	cd Kong && $(MAKE) test

clean:
	@echo "=== Cleaning all projects ==="
	cd LispInterpreter && $(MAKE) clean || true
	cd Kong && $(MAKE) clean || true

fclean:
	@echo "=== Deep cleaning all projects ==="
	cd LispInterpreter && $(MAKE) fclean || true
	cd Kong && $(MAKE) fclean || true

re: fclean all

.PHONY: all lisp kong clean fclean re test

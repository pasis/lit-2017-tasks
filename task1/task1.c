/* task1.c
 * Evaluates math expression.
 *
 * Author: Dmitry Podgorny <pasis.ua@gmail.com>
 */
/* gcc -lm -o task1 task1.c */

#include <assert.h>	/* assert */
#include <ctype.h>	/* tolower */
#include <math.h>	/* pow, sqrt, fabs, trunc */
#include <stdbool.h>	/* bool */
#include <stdio.h>	/* gets, printf */
#include <stdlib.h>	/* strtod */
#include <string.h>	/* strlen, strncmp */

#define N 255 /* XXX */

static double calc(const char *s, double x, int *offset, bool *error);

static bool is_op(char c)
{
	return c == '+' || c == '-' || c == '*' || c == '/' || c == '^';
}

static bool is_closing(const char *s, int i)
{
	if (s[i] == '|') {
		for (; s[i] == '|'; ++i);
		return s[i] == '\0' || is_op(s[i]) || is_closing(s, i);
	} else
		return s[i] == ')' || s[i] == ']';
}

static bool is_opening(const char *s, int i)
{
	return s[i] == '(' || s[i] == '[' || (s[i] == '|' && !is_closing(s, i));
}

static bool is_same(char c1, char c2)
{
	return (c1 == '(' && c2 == ')') ||
	       (c1 == '[' && c2 == ']') ||
	       (c1 == '|' && c2 == '|');
}

/* Removes spaces and converts letters to lower case. */
void normalize(char *s)
{
	int i;
	int j;

	for (i = 0, j = 0; s[i] != '\0'; ++i)
		if (s[i] != ' ')
			s[j++] = tolower(s[i]);
	s[j] = '\0';
}

/* Checks if brackets sequence is valid. */
static bool brackets_seq_is_valid(const char *s)
{
	size_t  len = strlen(s);
	char   *stack;
	int     top = 0;
	int     i;

	stack = malloc(len / 2);
	assert(stack != NULL);
	for (i = 0; i < len; ++i) {
		if (is_opening(s, i)) {
			if (top == len / 2)
				break;
			stack[top++] = s[i];
		}
		if (is_closing(s, i) &&
		    (top == 0 || !is_same(stack[--top], s[i])))
			break;
	}
	free(stack);

	return i == len && top == 0;
}

/* Checks if expression is valid. */
bool expr_is_valid(const char *s)
{
	return brackets_seq_is_valid(s);
}

/*
 * Evaluates the first element in `s'. An element is either
 *  - float number
 *  - (expression)
 *  - |expression|
 *  - [expression]
 *  - sqrt(expression)
 *  - element followed by ^ and an element
 *
 *  Length of the element is stored in `offset'
 */
static double element_eval(const char *s, double x, int *offset, bool *error)
{
	double  num;
	char   *endptr;
	bool    is_num;
	int     o;
	int     o_pow;

	assert(offset != NULL);

	if (s[0] == 'x') {
		num = x;
		o = 1;
	} else if (is_opening(s, 0)) {
		num = calc(s + 1, x, &o, error);
		if (s[0] == '|')
			num = fabs(num);
		if (s[0] == '[')
			num = trunc(num);
		++o; /* Count the opening bracket. */
	} else {
		num = strtod(s, &endptr);
		is_num = num != 0 || endptr != s;
		o = endptr - s;
		if (!is_num) {
			/* We support only `sqrt' as an example. */
			if (strncmp("sqrt", s, 4) == 0) {
				num = element_eval(s + 4, x, &o, error);
				if (!*error && num < 0)
					*error = true;
				else
					num = sqrt(num);
				o += 4;
			} else {
				assert(false); /* Invalid expression */
			}
		}
	}
	if (!*error && s[o] == '^') {
		/*
		 * In this way we reduce complexity of calc().
		 * Evaluate expression `x^y' as a single element.
		 */
		num = pow(num, element_eval(&s[o + 1], x, &o_pow, error));
		o += o_pow + 1;
	}

	*offset = o;
	return num;
}

/*
 * Recursive function of evaluation. Function stops on `)' or "end of line".
 * Length of evaluated part (including `)') is stored in `offset'.
 */
static double calc(const char *s, double x, int *offset, bool *error)
{
	double  left  = 0;
	double  right = 0;
	double  num;
	char    op  = '+';
	char    rop = 0;
	int     i;
	int     o;

	/*
	 * Explanation:
	 *  `left' and `right' contain left and right parts on lower-priority
	 *  binary operations + and -. `Right' accumulates result of higher-
	 *  priority operations * and /.
	 *
	 *  Lets look at expression as follows:
	 *  expr    ::= element | element op expr
	 *  op      ::= + | - | * | /
	 *  element ::= number | element^element | (expr) | sqrt(expr)
	 *
	 *  Lets merge adjacent elements with operations * / into single groups
	 *  `expr_mul_div'. So we have the next view:
	 *  [expr_mul_div] +/- [expr_mul_div] +/- [expr_mul_div]
	 *
	 *  Main idea is shown in the next lines:
	 *      0  +  [expr_mul_div] +/- [expr_mul_div] +/- [expr_mul_div]
	 *     ___    ______________
	 *  1: left      right
	 *     _____________________     ______________
	 *  2:        left                    right
	 *     ________________________________________     ______________
	 *  3:                  left                             right
	 *     ___________________________________________________________
	 *  4:                         left (answer)
	 */

	i = 0;
	while (s[i] != '\0' && !is_closing(s, i)) {
		/* Take a number */
		num = element_eval(&s[i], x, &o, error);
		i += o;
		if (*error)
			break;

		/* After a number we allow only operations and "the end" */
		assert(is_op(s[i]) || is_closing(s, i) || s[i] == '\0');

		/* *,/ have higher priority */
		if (rop == '*') {
			right *= num;
		} else if (rop == '/') {
			if (num == 0) {
				*error = true;
				break;
			}
			right /= num;
		} else
			right = num;
		rop = 0;

		/* Perform lower priority operations */
		if (s[i] != '*' && s[i] != '/') {
			assert(op == '+' || op == '-');
			if (op == '+')
				left += right;
			if (op == '-')
				left -= right;
		}

		/* Check the next operation */
		if (s[i] == '+' || s[i] == '-')
			op = s[i++];
		else if (s[i] == '*' || s[i] == '/')
			rop = s[i++];
	}

	if (offset != NULL) {
		assert(is_closing(s, i) || *error);
		*offset = i + 1;
	}
	return left;
}

double eval(const char *expr, double x, bool *error)
{
	*error = false;
	return calc(expr, x, NULL, error);
}

int main()
{
	char    expr[N];
	char   *s;
	double  res;
	bool    error;

	s = gets(expr);
	assert(s == expr);
	normalize(expr);
	assert(expr_is_valid(expr));
	res = eval(expr, 0, &error);
	if (error)
		printf("NaN\n");
	else
		printf("%lf\n", res);

	return 0;
}

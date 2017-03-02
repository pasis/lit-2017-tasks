/* task1.c
 * Evaluates math expression.
 *
 * Author: Dmitry Podgorny <pasis.ua@gmail.com>
 */
/* gcc -lm -o task1 task1.c */

#include <assert.h>	/* assert */
#include <math.h>	/* pow, sqrt */
#include <stdbool.h>	/* bool */
#include <stdio.h>	/* scanf, printf */
#include <stdlib.h>	/* strtod */
#include <string.h>	/* strlen, strncmp */
#include <unistd.h>	/* exit */

#define N 255

static double calc(const char *s, int *offset);

static void normalize(char *s)
{
	int i;
	int j;

	for (i = 0, j = 0; s[i] != '\0'; ++i)
		if (s[i] != ' ')
			s[j++] = s[i];
	s[j] = '\0';
}

static bool brackets_seq_is_valid(const char *s)
{
	size_t len = strlen(s);
	int    c   = 0;
	int    i;

	for (i = 0; i < len && c >= 0; ++i)
		if (s[i] == '(')
			++c;
		else if (s[i] == ')')
			--c;
	return i == len && c == 0;
}

static bool expr_is_valid(const char *s)
{
	return brackets_seq_is_valid(s);
}

/*
 * Evaluates the first element in `s'. An element is either
 *  - float number
 *  - (expression)
 *  - |expression|
 *  - sqrt(expression)
 *  - element followed by ^ and an element
 *
 *  Length of the element is stored in `offset'
 */
static double element_eval(const char *s, int *offset)
{
	double  num;
	char   *endptr;
	bool    is_num;
	int     o;
	int     o_pow;

	assert(offset != NULL);

	if (s[0] == '(' || s[0] == '|') {
		num = calc(s + 1, &o);
		if (s[0] == '|')
			num = fabs(num);
		++o;
	} else {
		num = strtod(s, &endptr);
		is_num = num != 0 || endptr != s;
		o = endptr - s;
		if (!is_num) {
			/* We support only `sqrt' as an example. */
			if (strncmp("sqrt", s, 4) == 0) {
				num = element_eval(s + 4, &o);
				assert(num >= 0);
				num = sqrt(num);
				o += 4;
			} else {
				assert(false); /* Invalid expression */
			}
		}
	}
	if (s[o] == '^') {
		/*
		 * In this way we reduce complexity of calc().
		 * Evaluate expression `x^y' as a single element.
		 */
		num = pow(num, element_eval(&s[o + 1], &o_pow));
		o += o_pow + 1;
	}

	*offset = o;
	return num;
}

/*
 * Recursive function of evaluation. Function stops on `)' or "end of line".
 * Length of evaluated part (including `)') is stored in `offset'.
 */
static double calc(const char *s, int *offset)
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
	while (s[i] != '\0' && s[i] != ')' && (s[i] != '|' || i == 0)) {
		/* Take a number */
		num = element_eval(&s[i], &o);
		i += o;

		/* After a number we allow only operations and "the end" */
		assert(s[i] == '+' || s[i] == '-' || s[i] == '*' ||
		       s[i] == '/' || s[i] == ')' || s[i] == '|' ||
		       s[i] == '\0');

		/* *,/ have higher priority */
		if (rop == '*') {
			right *= num;
			rop = 0;
		} else if (rop == '/') {
			if (num == 0) {
				fprintf(stderr, "Division by zero\n");
				exit(1);
			}
			right /= num;
			rop = 0;
		} else
			right = num;

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
		assert(s[i] == ')' || s[i] == '|');
		*offset = i + 1;
	}
	return left;
}

double eval(const char *expr)
{
	return calc(expr, NULL);
}

int main()
{
	char   expr[N];
	double res;

	scanf("%s", expr);
	normalize(expr);
	assert(expr_is_valid(expr));
	res = eval(expr);
	printf("%lf\n", res);

	return 0;
}

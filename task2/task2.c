/* task2.c
 * Draw a graph of the function.
 *
 * Author: Dmitry Podgorny <pasis.ua@gmail.com>
 */
/* gcc -lm `sdl2-config --cflags --libs` -o task2 task2.c */

#include <assert.h>	/* assert */
#include <math.h>	/* modf, trunc */
#include <stdbool.h>	/* bool */
#include <stdio.h>	/* gets */
#include <SDL.h>

#define main main_void
#include "../task1/task1.c"	/* eval, normalize, expr_is_valid */
#undef main

static void draw_graph(const char *expr)
{
	double res;
	double iptr;
	double x;
	bool   error;
	bool   draw = false;
	int    xi;
	int    yi;
	int    x_old;
	int    y_old;
	int    i;
	int    count;

	SDL_Window   *window;
	SDL_Renderer *renderer;
	SDL_Event     e;
	const int     width  = 1024;
	const int     height = 768;
	bool          quit   = false;

	const double step = 0.05;
	const double from = trunc(step * width / 2) * -1;
	const double to   = trunc(step * width / 2);

	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
	SDL_CreateWindowAndRenderer(width, height, 0, &window, &renderer);
	assert(window != NULL);
	assert(renderer != NULL);
	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
	SDL_RenderClear(renderer);
	SDL_SetRenderDrawColor(renderer, 128, 128, 128, 128);
	for (xi = 0; xi < width; ++xi) {
		SDL_RenderDrawPoint(renderer, xi, height / 2);
		if (modf(step * (width / 2 - xi), &iptr) == 0) {
			SDL_RenderDrawPoint(renderer, xi, height / 2 - 1);
			SDL_RenderDrawPoint(renderer, xi, height / 2 + 1);
		}
	}
	for (yi = 0; yi < height; ++yi) {
		SDL_RenderDrawPoint(renderer, width / 2, yi);
		if (modf(step * (height / 2 - yi), &iptr) == 0) {
			SDL_RenderDrawPoint(renderer, width / 2 - 1, yi);
			SDL_RenderDrawPoint(renderer, width / 2 + 1, yi);
		}
	}
	SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);

	count = (int)trunc((to - from) / step);
	for (i = 0; i < count; ++i) {
		x = from + step * i;
		res = eval(expr, x, &error);
		if (!error) {
			xi = width / 2 + (int)round(x / step);
			yi = height / 2 - (int)round(res / step);
			SDL_RenderDrawPoint(renderer, xi, yi);
			if (draw)
				SDL_RenderDrawLine(renderer, xi, yi, x_old, y_old);
			x_old = xi;
			y_old = yi;
			draw = true;
		} else
			draw = false;
	}
	SDL_RenderPresent(renderer);

	while (!quit) {
		if (!SDL_WaitEvent(&e))
			continue;
		switch (e.type) {
		case SDL_QUIT:
		case SDL_KEYDOWN:
			quit = true;
			break;
		}
	}
	SDL_Quit();
}

int main()
{
	char  expr[N];
	char *s;

	printf("f(x) = ");
	s = gets(expr);
	assert(s == expr);
	normalize(expr);
	assert(expr_is_valid(expr));
	draw_graph(expr);

	return 0;
}

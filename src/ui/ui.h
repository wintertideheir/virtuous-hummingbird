#include "draw/color.h"

struct UIElement;

struct UIElement *uielement_border(struct UIElement* element, int x, int y);
struct UIElement *uielement_fixed(struct UIElement* element, int x, int y);
struct UIElement *uielement_vertical(int length, ...);
struct UIElement *uielement_horizontal(int length, ...);
struct UIElement *uielement_text(const char* text);
struct UIElement *uielement_button(void (*callback)(), struct RGBA color);

void uielement_generate(struct UIElement *element);
void uielement_draw(struct UIElement* element);

void uielement_destructor(struct UIElement *element);

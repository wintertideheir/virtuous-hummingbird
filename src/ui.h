#include "color.h"

struct UIElement;

struct UIElement *uielement_border(struct UIElement* element, int x, int y);
struct UIElement *uielement_fixed(struct UIElement* element, int x, int y);
struct UIElement *uielement_vertical(int length, ...);
struct UIElement *uielement_horizontal(int length, ...);
struct UIElement *uielement_text(struct RGBA color, const char* text);
struct UIElement *uielement_box(struct RGBA color, struct UIElement *element);

void uielement_update(struct UIElement *element);
void uielement_draw(struct UIElement* element);

void uielement_destructor(struct UIElement *element);

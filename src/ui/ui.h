struct UIElement;

struct UIElement *uielement_vertical(int length, ...);
struct UIElement *uielement_horizontal(int length, ...);
struct UIElement *uielement_text(const char* text);
struct UIElement *uielement_button(void (*callback)(), float R, float G, float B, float A);

void uielement_generate(struct UIElement *element);
void uielement_draw(struct UIElement* element);

void uielement_destructor(struct UIElement *element);

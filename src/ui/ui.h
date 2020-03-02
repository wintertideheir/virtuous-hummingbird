struct UIElement;

struct UIElement *uielement_vertical(int length, ...);
struct UIElement *uielement_horizontal(int length, ...);
struct UIElement *uielement_text(const char* text);
struct UIElement *uielement_button(void (*callback)());

void uielement_destructor(struct UIElement *element);
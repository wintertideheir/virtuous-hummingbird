struct FloatTuple
{
    float x;
    float y;
};
struct UIElement;

struct UIElement *uielement_scaled(struct UIElement *elements, struct FloatTuple scale);
struct UIElement *uielement_vertical(int length, ...);
struct UIElement *uielement_horizontal(int length, ...);
struct UIElement *uielement_text(const char* text);
struct UIElement *uielement_button();

void uielement_destructor(struct UIElement *element);
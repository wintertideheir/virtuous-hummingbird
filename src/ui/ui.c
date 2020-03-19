#include "ui.h"

#include "draw/window.h"
#include "draw/shapes.h"

#include <stdlib.h>
#include <stdarg.h>

struct UIElementFixed
{
    struct UIElement *element;
    int x;
    int y;
};

struct UIElementArray
{
    int              elements_length;
    struct UIElement **elements;
    int              *elements_sizes;
};

struct UIElementButton
{
    void         (*callback)();
    unsigned int VAO;
    unsigned int VBO;
    struct RGBA  color;
};

enum UIElementType
{
    UIELEMENT_BORDER,
    UIELEMENT_FIXED,
    UIELEMENT_VERTICAL,
    UIELEMENT_HORIZONTAL,
    UIELEMENT_TEXT,
    UIELEMENT_BUTTON
};

union UIElementValue
{
    struct UIElementArray  array;
    const char             *text;
    struct UIElementButton button;
    struct UIElementFixed  fixed;
};

struct UIElement
{
    enum UIElementType   type;
    union UIElementValue value;
};

struct UIElement *uielement_border(struct UIElement* element, int x, int y)
{
    struct UIElement *e = malloc(sizeof(struct UIElement));
    e->type = UIELEMENT_BORDER;
    e->value.fixed.element = element;
    e->value.fixed.x = x;
    e->value.fixed.y = y;
    return e;
}

struct UIElement *uielement_fixed(struct UIElement* element, int x, int y)
{
    struct UIElement *e = malloc(sizeof(struct UIElement));
    e->type = UIELEMENT_FIXED;
    e->value.fixed.element = element;
    e->value.fixed.x = x;
    e->value.fixed.y = y;
    return e;
}

struct UIElement *uielement_array(va_list va, int length)
{
    struct UIElement *e              = malloc(sizeof(struct UIElement));
    struct UIElement **elements      = malloc(sizeof(struct UIElement[length]));
    int              *elements_sizes = malloc(sizeof(int[length]));
    for (int i = 0; i < length; i++)
    {
        elements[i]       = va_arg(va, struct UIElement*);
        elements_sizes[i] = va_arg(va, int);
    }
    e->value.array =
        (struct UIElementArray)
            {.elements_length = length,
             .elements        = elements,
             .elements_sizes  = elements_sizes};
    return e;
}

struct UIElement *uielement_vertical(int length, ...)
{
    va_list va;
    va_start(va, length);
    struct UIElement *e = uielement_array(va, length);
    va_end(va);

    e->type = UIELEMENT_VERTICAL;
    return e;
}

struct UIElement *uielement_horizontal(int length, ...)
{
    va_list va;
    va_start(va, length);
    struct UIElement *e = uielement_array(va, length);
    va_end(va);

    e->type = UIELEMENT_HORIZONTAL;
    return e;
}

struct UIElement *uielement_text(const char* text)
{
    struct UIElement *e = malloc(sizeof(struct UIElement));
    e->type       = UIELEMENT_TEXT;
    e->value.text = text;
    return e;
}

struct UIElement *uielement_button(void (*callback)(), struct RGBA color)
{
    struct UIElement *e = malloc(sizeof(struct UIElement));
    e->type            = UIELEMENT_BUTTON;
    e->value.button.callback = callback;
    e->value.button.color = color;
    return e;
}

void uielement_generate_partial(struct UIElement *element,
                                float upper_x, float lower_x,
                                float upper_y, float lower_y,
                                int layer)
{
    switch (element->type)
    {
        case UIELEMENT_BORDER:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            float fixed_x = (float) element->value.fixed.x / (float) windowX;
            float fixed_y = (float) element->value.fixed.y / (float) windowY;
            uielement_generate_partial(element->value.fixed.element,
                                       upper_x - fixed_x, lower_x + fixed_x,
                                       upper_y - fixed_y, lower_y + fixed_y,
                                       layer + 1);
            break;
        case UIELEMENT_FIXED:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            float center_x = (upper_x + lower_x) / 2;
            float center_y = (upper_y + lower_y) / 2 ;
            fixed_x = (float) element->value.fixed.x / (float) windowX;
            fixed_y = (float) element->value.fixed.y / (float) windowY;
            uielement_generate_partial(element->value.fixed.element,
                                       center_x + fixed_x, center_x - fixed_x,
                                       center_y + fixed_y, center_y - fixed_y,
                                       layer + 1);
            break;
        case UIELEMENT_VERTICAL:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            float total_size = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                total_size += element->value.array.elements_sizes[i];
            }
            float specific_bound = (upper_y - lower_y)/total_size;
            float current_bound = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                uielement_generate_partial(element->value.array.elements[i],
                                           upper_x, lower_x,
                                           lower_y + current_bound + (specific_bound * element->value.array.elements_sizes[i]),
                                           lower_y + current_bound,
                                           layer + 1);
                current_bound += specific_bound * element->value.array.elements_sizes[i];
            }
            break;
        case UIELEMENT_HORIZONTAL:
            ; // "A label can only be part of a statement and a declaration is not a statement"
            total_size = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                total_size += element->value.array.elements_sizes[i];
            }
            specific_bound = (upper_x - lower_x)/total_size;
            current_bound = 0;
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                uielement_generate_partial(element->value.array.elements[i],
                                           lower_x + current_bound + (specific_bound * element->value.array.elements_sizes[i]),
                                           lower_x + current_bound,
                                           upper_y, lower_y, layer + 1);
                current_bound += specific_bound * element->value.array.elements_sizes[i];
            }
            break;
        case UIELEMENT_TEXT:
            break;
        case UIELEMENT_BUTTON:
            shapesGenerateBox(upper_x, lower_x, upper_y, lower_y,
                              layer, element->value.button.color,
                              &element->value.button.VAO, &element->value.button.VBO);
            break;
    }
}

void uielement_generate(struct UIElement *element)
{
    uielement_generate_partial(element, 1.0, -1.0, 1.0, -1.0, 0);
}

void uielement_draw(struct UIElement *element)
{
    switch (element->type)
    {
        case UIELEMENT_BORDER:
        case UIELEMENT_FIXED:
            uielement_draw(element->value.fixed.element);
            break;
        case UIELEMENT_VERTICAL:
        case UIELEMENT_HORIZONTAL:
            for(int i = 0; i < element->value.array.elements_length; i++)
                uielement_draw(element->value.array.elements[i]);
            break;
        case UIELEMENT_BUTTON:
            shapesDrawBox(&element->value.button.VAO);
            break;
        case UIELEMENT_TEXT:
            break;
    }
}

void uielement_destructor(struct UIElement *element)
{
    switch (element->type)
    {
        case UIELEMENT_BORDER:
        case UIELEMENT_FIXED:
            uielement_destructor(element->value.fixed.element);
            free(element);
            break;
        case UIELEMENT_VERTICAL:
        case UIELEMENT_HORIZONTAL:
            for (int i = 0; i < element->value.array.elements_length; i++)
            {
                uielement_destructor(element->value.array.elements[i]);
                free(element->value.array.elements);
                free(element->value.array.elements_sizes);
            }
            free(element);
            break;
        case UIELEMENT_TEXT:
        case UIELEMENT_BUTTON:
            free(element);
            break;
        default:
            break;
    }
}

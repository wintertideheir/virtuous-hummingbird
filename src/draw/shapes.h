#include "color.h"

void shapesBegin();
void shapesGenerateBox(float upper_x, float lower_x, float upper_y, float lower_y,
                       float layer, struct RGBA color,
                       unsigned int *VAO, unsigned int *VBO);
void shapesDrawBox(unsigned int *VAO);

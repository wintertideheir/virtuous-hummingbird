#include "color.h"

void shapesBegin();
void shapesGenerateBox(float upper_x, float lower_x, float upper_y, float lower_y,
                       float layer, struct RGBA color,
                       unsigned int *VAO, unsigned int *VBO);
void shapesGenerateText(float upper_x, float lower_x, float upper_y, float lower_y,
                        float layer, struct RGBA color,
                        char* bitmap, int bitmap_width, int bitmap_height,
                        unsigned int* TID, unsigned int *VAO, unsigned int *VBO);
void shapesUpdateBox(float upper_x, float lower_x, float upper_y, float lower_y,
                     float layer, struct RGBA color, unsigned int *VBO);
void shapesUpdateText(float upper_x, float lower_x, float upper_y, float lower_y,
                      float layer, struct RGBA color, unsigned int *VBO);
void shapesDrawBox(unsigned int *VAO);
void shapesDrawText(unsigned int *TID, unsigned int *VAO);

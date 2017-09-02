#pragma once

#include "resource.h"
#include "stdafx.h"
#include "cstring"
#include "cstdlib"
#include "cmath"

const int dir4x[4] = {0, 0, 1, -1};
const int dir4y[4] = {1, -1, 0, 0};

class Point{
public:	int x, y;
};

class Bitmap
{
public:
	int row, col;
	int *mtr;
	Bitmap(){
		row = col = 0;
		mtr = NULL;
	}
};

int **DualMalloc(int row, int col);
void DualFree(int **p, int row);
int GenerateVein(Bitmap flow, int **dis, int **hold);

Bitmap LeafVein(Bitmap lb, Point src)
{
	Bitmap vein;
	if (lb.row <= 0 || lb.col <= 0 || lb.mtr == NULL)
	{
		MessageBox(NULL, L"LeafVein():\nInput bitmap is not valid.", NULL, MB_OK);
		return vein;
	}

	if (src.x < 0) src.x = 0;
	if (src.x >= lb.col) src.x = lb.col - 1;
	if (src.y < 0) src.y = 0;
	if (src.y >= lb.row) src.y = lb.row - 1;
	if (lb.mtr[src.y*lb.col+src.x])
	{
		MessageBox(NULL, L"LeafVein():\nThe touch point is not blank.", NULL, MB_OK);
		return vein;
	}

	vein.row = lb.row;
	vein.col = lb.col;
	vein.mtr = new int[vein.row*vein.col];                                                     // new memory
	if (vein.mtr == NULL)
	{
ERROR_NEED_MORE_MEMORY:
		MessageBox(NULL, L"LeafVein():\nNeed more memory.", NULL, MB_OK);
		return vein;
	}
	memset(vein.mtr, 0, vein.row*vein.col*sizeof(int));
	
	int **dis = DualMalloc(vein.row, vein.col);                                   // new memory
	if (dis == NULL)
	{
DELETE_DIS:
		delete vein.mtr;
		vein = Bitmap();
		goto ERROR_NEED_MORE_MEMORY;
	}
	for (int i=0; i<vein.row; i++){
		memset(dis[i], 0, vein.col*sizeof(int));
	}

	int area = 1;
	dis[src.y][src.x] = 1;
	bool fld = false;
	while (fld == false)
	{
		fld = true;
		for (int i=0; i<vein.row; i++){
			for (int j=0; j<vein.col; j++) if (dis[i][j]){
				for (int dc=0; dc<4; dc++){
					if (
						i+dir4y[dc] >= 0 && i+dir4y[dc] < vein.row && j+dir4x[dc] >= 0 && j+dir4x[dc] < vein.col
						&& dis[i+dir4y[dc]][j+dir4x[dc]] == 0
						&& lb.mtr[(i+dir4y[dc])*vein.col + (j+dir4x[dc])] == 0
						)
					{
						dis[i+dir4y[dc]][j+dir4x[dc]] = dis[i][j] + 1;
						area++;
						fld = false;
					}
				}
			}
		}
	}

	if (area < 40)
		MessageBox(NULL, L"LeafVein():\nThe target area is smaller than 40 pixels.", L"Warning", MB_OK);

	// core part
	int **hold = DualMalloc(vein.row, vein.col);
	if (hold == NULL)
	{
DELETE_HOLD:
		DualFree(dis, vein.row);
		goto DELETE_DIS;
	}

	for (int i=0; i<vein.row; i++){
		for (int j=0; j<vein.col; j++){
			hold[i][j] = (dis[i][j] != 0);
		}
	}

#define DIRECT_DIS
#ifdef DIRECT_DIS
	int ddmaxdis = 0;
	for (int i=0; i<vein.row; i++){
		for (int j=0; j<vein.col; j++){
			dis[i][j] = sqrt(double((i-src.y)*(i-src.y) + (j-src.x)*(j-src.x)));
			if (dis[i][j] > ddmaxdis) ddmaxdis = dis[i][j];
		}
	}

	for (int i=0; i<vein.row; i++){
		for (int j=0; j<vein.col; j++){
			dis[i][j] = sqrt(double((i-src.y)*(i-src.y) + (j-src.x)*(j-src.x))) / ddmaxdis * 10e5;
		}
	}
#endif

	if (GenerateVein(vein, dis, hold) == 1)
	{
		DualFree(hold, vein.row);
		goto DELETE_HOLD;
	}
	// core part end

	return vein;
}

int GenerateVein(Bitmap flow, int **dis, int **hold)
{
	int **nhold = DualMalloc(flow.row, flow.col);
	if (nhold == NULL)
		return NULL;

	srand(GetTickCount());
	int epn = 1;
	int lpcn = 5 * (flow.row + flow.col);
	while (--lpcn && epn)
	{
		epn = 0;
		for (int i=0; i<flow.row; i++){
			for (int j=0; j<flow.col; j++){
				nhold[i][j] = 0;
			}
		}

		for (int i=0; i<flow.row; i++){
			for (int j=0; j<flow.col; j++) if (hold[i][j])
			{
				epn++;
				int ndir = -1;
				int maxflow;
				int mindis;
				int ni, nj;
				for (int k=0; k<4; k++)
				{
					ni = i + dir4y[k];
					nj = j + dir4x[k];

					if (
						ni >= 0 && ni < flow.row && nj >= 0 && nj < flow.col
						&& dis[ni][nj] != 0
						&& dis[ni][nj] <= dis[i][j]
						&& (ndir < 0 || flow.mtr[ni*flow.col+nj] > 0.7 * maxflow)
						)
					{
						if (ndir < 0) goto LOCAL_JUMP_01;
						double imp = 10;
						int value = (double(flow.mtr[ni*flow.col+nj]) / maxflow) + imp * (double(mindis) / dis[ni][nj]);
						double dif = 0.1;
						if (ndir < 0 || value > (1.0+imp)*(1.0+dif) || value > (1.0+imp)*(1.0-dif) && (rand()&1))
						{
LOCAL_JUMP_01:
							ndir = k;
							maxflow = flow.mtr[ni*flow.col+nj];
							mindis = dis[ni][nj];
						}
					}
				}

				if (ndir < 0)
					continue;

				ni = i + dir4y[ndir];
				nj = j + dir4x[ndir];

				nhold[ni][nj] += hold[i][j];
				flow.mtr[ni*flow.col + nj] += hold[i][j];
			}
		}

		int **tp;
		tp = hold;
		hold = nhold;
		nhold = tp;
	}

	if (lpcn == 0)
		MessageBox(NULL, L"LeafVein():\nAbort core loop because time limit exceeded.", L"Warning", MB_OK);

	DualFree(nhold, flow.row);
	return 0;
}

int **DualMalloc(int row, int col)
{
	int **p = new int *[row];
	if (p == NULL)
		return NULL;

	int i;
	for (i=0; i<row; i++)
	{
		p[i] = new int[col];
		if (p[i] == NULL)
			break;
	}

	if (i < row){
		for (; i>=0; i--)
			delete p[i];
		delete p;
		return NULL;
	}

	return p;
}

void DualFree(int **p, int row)
{
	for (int i=0; i<row; i++)
		delete p[i];
	delete p;
}

/****************************************************************************
 * Copyright (C) 2014 by Taylor Arnold, Ryan Tibshirani, Veerun Sadhanala   *
 *                                                                          *
 * This file is part of the glmgen library / package.                       *
 *                                                                          *
 *   glmgen is free software: you can redistribute it and/or modify it      *
 *   under the terms of the GNU Lesser General Public License as published  *
 *   by the Free Software Foundation, either version 2 of the License, or   *
 *   (at your option) any later version.                                    *
 *                                                                          *
 *   glmgen is distributed in the hope that it will be useful,              *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 *   GNU Lesser General Public License for more details.                    *
 *                                                                          *
 *   You should have received a copy of the GNU Lesser General Public       *
 *   License along with glmgen. If not, see <http://www.gnu.org/licenses/>. *
 ****************************************************************************/

/**
 * @file tf.h
 * @author Taylor Arnold, Ryan Tibshirani, Veerun Sadhanala
 * @date 2014-12-23
 * @brief Main functions for fitting trend filtering models.
 *
 * Here.
 */

#ifndef TF_H
#define TF_H

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


/* Dynamic programming routines */
SEXP bflsa(SEXP Rindx, SEXP Ry, SEXP Rlam);
SEXP bwflsa(SEXP Rindx, SEXP Ry, SEXP Rw, SEXP Rlam);
void tf_dp (int n, double *y, double lam, double *beta);
void tf_dp_weight (int n, double *y, double *w, double lam, double *beta);


#endif

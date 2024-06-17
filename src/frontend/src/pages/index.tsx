import { Spin } from 'antd'
import { t } from 'i18next'
import { Suspense, lazy } from 'react'
import { Outlet, RouteObject } from 'react-router-dom'
import {
  ROUTE_PATH_INDEX,
  ROUTE_PATH_SIGN_IN,
  ROUTE_PATH_SIGN_UP,
  ROUTE_PATH_SOMETHING,
  ROUTE_PATH_LOREM,
  ROUTE_PATH_SOMETHING_NEW,
  ROUTE_PATH_LESSONS,
  ROUTE_PATH_LESSON,
} from '~/shared/routing'

export const routes: RouteObject[] = [
  {
    element: (
      <Suspense fallback={<Spin fullscreen />}>
        <Outlet />
      </Suspense>
    ),
    children: [
      {
        path: ROUTE_PATH_INDEX,
        Component: lazy(() => import('./PageSomething')),
      },
      {
        path: ROUTE_PATH_SIGN_IN,
        Component: lazy(() => import('./PageSignIn')),
      },
      {
        path: ROUTE_PATH_SIGN_UP,
        Component: lazy(() => import('./PageSignUp')),
      },
      {
        path: ROUTE_PATH_LESSONS,
        children: [
          {
            index: true,
            Component: lazy(() => import('./PageLessons')),
          },
          {
            path: ROUTE_PATH_LESSON,
            Component: lazy(() => import('./PageLesson')),
          },
        ],
      },
      {
        path: ROUTE_PATH_SOMETHING,
        handle: {
          crumb: () => ({
            title: t('PageSomething:title'),
          }),
        },
        children: [
          {
            index: true,
            Component: lazy(() => import('./PageSomething')),
          },
          {
            path: ROUTE_PATH_SOMETHING_NEW,
            Component: lazy(() => import('./PageSomethingNew')),
            handle: {
              crumb: () => ({
                title: t('PageSomethingNew:title'),
              }),
            },
          },
        ],
      },
      {
        path: ROUTE_PATH_LOREM,
        Component: lazy(() => import('./PageLorem')),
        handle: {
          crumb: () => ({
            title: 'Lorem',
          }),
        },
      },
    ],
  },
]

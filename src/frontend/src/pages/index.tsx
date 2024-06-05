import { Spin } from 'antd'
import { Suspense, lazy } from 'react'
import { Outlet, RouteObject } from 'react-router-dom'
import {
  ROUTE_PATH_INDEX,
  ROUTE_PATH_LOGIN,
  ROUTE_PATH_REGISTER,
  ROUTE_PATH_SOMETHING,
  ROUTE_PATH_LOREM,
  ROUTE_PATH_SOMETHING_NEW,
} from '~/shared/routng/constants'

export const routesConfig: RouteObject[] = [
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
        path: ROUTE_PATH_LOGIN,
        Component: lazy(() => import('./PageLogin')),
      },
      {
        path: ROUTE_PATH_REGISTER,
        Component: lazy(() => import('./PageRegister')),
      },
      {
        path: ROUTE_PATH_SOMETHING,
        Component: lazy(() => import('./PageSomething')),
      },
      {
        path: ROUTE_PATH_SOMETHING_NEW,
        Component: lazy(() => import('./PageSomethingNew')),
      },
      {
        path: ROUTE_PATH_LOREM,
        Component: lazy(() => import('./PageLorem')),
      },
    ],
  },
]

import '~/shared/intl'

import { App as AntApp, ConfigProvider } from 'antd'
import React from 'react'
import { RouterProvider, createBrowserRouter } from 'react-router-dom'
import { routes } from '~/pages'
import { defaultTheme } from './styles/themes'
import { AuthProvider } from './ui/AuthProvider'

const router = createBrowserRouter(routes)

export function App() {
  return (
    <React.StrictMode>
      <ConfigProvider theme={defaultTheme}>
        <AntApp>
          <AuthProvider>
            <RouterProvider router={router} />
          </AuthProvider>
        </AntApp>
      </ConfigProvider>
    </React.StrictMode>
  )
}

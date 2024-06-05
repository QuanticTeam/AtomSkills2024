import { ConfigProvider } from 'antd'
import React from 'react'
import { RouterProvider, createBrowserRouter } from 'react-router-dom'
import { defaultTheme } from './styles/themes'
import { AuthProvider } from './ui/AuthProvider'
import { routesConfig } from '~/pages'

const router = createBrowserRouter(routesConfig)

export function App() {
  return (
    <React.StrictMode>
      <ConfigProvider theme={defaultTheme}>
        <AuthProvider>
          <RouterProvider router={router} />
        </AuthProvider>
      </ConfigProvider>
    </React.StrictMode>
  )
}

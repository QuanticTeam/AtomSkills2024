import '~/shared/intl'

import React from 'react'
import { QueryClientProvider } from '@tanstack/react-query'
import { App as AntApp, ConfigProvider } from 'antd'
import { RouterProvider, createBrowserRouter } from 'react-router-dom'
import { routes } from '~/pages'
import { queryClient } from '~/shared/queryClient'
import { defaultTheme } from './styles/themes'
import { AuthProvider } from './ui/AuthProvider'

const router = createBrowserRouter(routes)

export function App() {
  return (
    <React.StrictMode>
      <QueryClientProvider client={queryClient}>
        <ConfigProvider theme={defaultTheme}>
          <AntApp>
            <AuthProvider>
              <RouterProvider router={router} />
            </AuthProvider>
          </AntApp>
        </ConfigProvider>
      </QueryClientProvider>
    </React.StrictMode>
  )
}

import { Layout } from 'antd'
import { ReactNode } from 'react'
import { GuardUnauthorized } from '~/shared/auth'
import { Footer } from '~/widgets'

interface PageUnauthorizedProps {
  children: ReactNode
}

export function PageUnauthorized({ children }: PageUnauthorizedProps) {
  return (
    <GuardUnauthorized>
      <Layout className="h-screen">
        <Layout.Content>
          <div className="bg-gray-100 flex flex-col min-h-full justify-center">{children}</div>
        </Layout.Content>

        <Layout.Footer>
          <Footer />
        </Layout.Footer>
      </Layout>
    </GuardUnauthorized>
  )
}

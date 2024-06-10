import { Layout } from 'antd'
import { ReactNode } from 'react'
import { GuardUnauthorized } from '~/shared/auth'
import { Logo } from '~/shared/ui'
import { Footer } from '~/widgets'

interface PageUnauthorizedProps {
  children: ReactNode
}

export function PageUnauthorized({ children }: PageUnauthorizedProps) {
  return (
    <GuardUnauthorized>
      <Layout className="h-screen">
        <Layout.Content>
          <div className="w-72 flex mx-auto mb-16">
            <Logo full />
          </div>

          <div className="bg-gray-100 flex flex-col min-h-full justify-center">
            <div className="flex">{children}</div>
          </div>
        </Layout.Content>

        <Layout.Footer>
          <Footer />
        </Layout.Footer>
      </Layout>
    </GuardUnauthorized>
  )
}

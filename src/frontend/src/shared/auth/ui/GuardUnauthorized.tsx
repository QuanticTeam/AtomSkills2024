import { ReactNode, useContext } from 'react'
import { Navigate } from 'react-router-dom'
import { ROUTE_PATH_SOMETHING } from '~/shared/routng/constants'
import { AuthContext } from './AuthContext'

interface GuardUnauthorizedProps {
  children: ReactNode
}

export function GuardUnauthorized({ children }: GuardUnauthorizedProps) {
  const { authenticated } = useContext(AuthContext)

  if (!authenticated) return <div>{children}</div>

  return (
    <Navigate
      to={ROUTE_PATH_SOMETHING}
      replace={true}
    />
  )
}
